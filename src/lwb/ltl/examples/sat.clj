; lwb Logic WorkBench -- Linear Temporal Logic: Examples Satisfiability of LTL

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.ltl.examples.sat
  (:require [lwb.ltl :refer :all]         ; needed for macroexpand-1 of xor etc !!
            [lwb.ltl.sat :refer :all]
            [lwb.ltl.buechi :as b]
            [lwb.ltl.kripke :as k]))

; Operators -------------------------------------------------------------------------------------------------

(def o01 '(not P))
(b/ba o01)
(sat? o01)
(sat o01)
(comment
  (k/texify (sat o01) "ks")
  )

(def o02 '(and P Q))
(b/ba o02)
(sat? o02)
(comment
  (k/texify (sat o02) "ks") 
  )

(def o03 '(or P Q))
(b/ba o03)
(sat? o03)
(comment
  (k/texify (sat o03) "ks")
  )

(def o04 '(impl P Q))
(b/ba o04)
(sat? o04)
(comment
  (k/texify (sat o04) "ks")
  )

(def o05 '(equiv P Q))
(b/ba o05)
(sat? o05)
(comment
  (k/texify (sat o05) "ks") 
  )

(def o06 '(xor P Q))
(b/ba o06)
(sat? o06)
(comment
  (k/texify (sat o06) "ks")  
  )

(def o07 '(ite P Q R))
(b/ba o07)
(sat? o07)
(comment
  (k/texify (sat o07) "ks") 
  )

(def o08 '(always P))
(b/ba o08)
(sat? o08)
(sat o08)
(comment
  (k/texify (sat o08) "ks")
  (k/texify (sat '(always (and P Q))) "ks")
  )

(def o09 '(finally P))
(b/ba o09)
(sat? o09)
(comment
  (k/texify (sat o09) "ks")
  (k/texify (sat '(finally (or P Q))) "ks")
  )

(def o10 '(atnext P))
(b/ba o10)
(sat? o10)
(comment
  (k/texify (sat o10) "ks")
  (k/texify (sat '(atnext (atnext P))) "ks")
  )

(def o11 '(until P Q))
(b/ba o11)
(sat? o11)
(comment
  (k/texify (sat o11) "ks")
  (k/texify (sat '(and P (until P Q))) "ks")
  (k/texify (sat '(and (until P Q) (atnext P))) "ks")
  )

(sat '(and (until P Q) (atnext P)))

(def o12 '(release P Q))
(sat? o12)
(comment
  (k/texify (sat o12) "ks")  
  )

; Theorems --------------------------------------------------------------------------

(def t1 '(impl (always P) P))

(sat? t1)      ; => true
(valid? t1)    ; => true
(comment
  (k/texify (sat t1) "ks")  
  )

(def t2 '(impl (always P) (atnext P)))
(sat? t2)      ; => true
(valid? t2)    ; => true
(comment
  (k/texify (sat t2) "ks") 
  )

(def t3 '(impl (always P) (finally P)))
(sat? t3)      ; => true
(valid? t3)    ; => true
(comment
  (k/texify (sat t3) "ks")
  )

(def t4 '(impl (always P) (always (always P))))
(sat? t4)      ; => true
(valid? t4)    ; => true
(comment
  (k/texify (sat t4) "ks")
  )

(def t5 '(impl (atnext P) (finally P)))
(sat? t5)      ; => true
(valid? t5)    ; => true
(comment
  (k/texify (sat t5) "ks") 
  )

; Typical formulas ---------------------------------------------------------------------------

; If P then finally Q
(def f1 '(impl P (finally Q)))
(sat? f1)      ; => true
(valid? f1)    ; => false
(comment
  (k/texify (sat f1) "ks") 
  )

; Infinitely often  P
(def f2 '(always (finally P)))
(sat? f2)       ; => true
(valid? f2)     ; => false
(b/ba f2)
(sat f2)
(comment
  (k/texify (sat f2) "ks") 
  )

(def f3 '(always (finally (not P))))
(sat? f3)       ; => true
(valid? f3)     ; => false
(comment
  (k/texify (sat f3) "ks") 
  )

; Finitely often (not P)
(def f4 '(finally (always P)))
(sat? f4)       ; => true
(valid? f4)     ; => false
(comment
  (k/texify (sat f4) "ks") 
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

; P and Q alternating
(def f7 '(and P (not Q) (always (impl P (atnext (and Q (not P))))) (always (impl Q (atnext (and P (not Q)))))))
(sat? f7)       ; => true
(valid? f7)     ; => false
(comment
  (k/texify (sat f7) "ks") ; first {P}  the always {Q} is a possible model
  )

; Fairness constraints -------------------------------------------------------

; unconditional fairness
(def ufair '(always (finally Q)))
(sat? ufair)    ; => true
(valid? ufair)  ; => false
(comment
  (k/texify (sat ufair) "ks")
  )

; strong fairness
(def sfair '(impl (always (finally P)) (always (finally Q))))
(sat? sfair)    ; => true
(valid? sfair)  ; => false
(comment
  (k/texify (sat sfair) "ks") ; not a very interesting model!!
  )

; weak fairness
(def wfair '(impl (finally (always P)) (always (finally Q))))
(sat? wfair)    ; => true
(valid? wfair)  ; => false
(comment
  (k/texify (sat wfair) "ks") ; not a very interesting model!!
  )

; fairness
(def fair (list 'and ufair sfair wfair))
(sat? fair)    ; => true
(valid? fair)  ; => false
(comment
  (k/texify (sat fair) "ks") ; see ufair
  )
