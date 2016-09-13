; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

; AND
{:name "and-i"
 :given      [a b]
 :conclusion [(and a b)]
 :forward   true
 :backward  true}
{:name "and-e1"
 :given      [(and a b)]
 :conclusion [a]
 :forward   true}
{:name "and-e2"
 :given      [(and a b)]
 :conclusion [b]
 :forward   true}

; OR
{:name "or-i1"
 :given      [a]
 :conclusion [(or a b)]
 :forward   true
 :backward  true}
{:name "or-i2"
 :given      [a]
 :conclusion [(or b a)]
 :forward   true
 :backward  true}
{:name "or-e"
 :given      [(or a b)
              (infer a X)
              (infer b X)]
 :conclusion [X]
 :backward  true}

; IMPL
{:name "impl-i"
 :given      [(infer a b)]
 :conclusion [(impl a b)]
 :backward   true}
{:name "impl-e"
 :given      [a (impl a b)]
 :conclusion [b]
 :forward   true
 :backward  true}

; NOT
{:name "not-i"
 :given      [(infer a contradiction)]
 :conclusion [(not a)]
 :backward   true}
{:name "not-e"
 :given      [a (not a)]
 :conclusion [contradiction]
 :forward   true
 :backward  true}

; RAA/CONTRA
{:name "raa"
 :given      [(infer (not a) contradiction)]
 :conclusion [a]
 :backward  true}
{:name "efq"
 :given      [contradiction]
 :conclusion [a]
 :forward   true
 :backward  true}
          
; EQUAL
{:name "equal-i"
 :given      []
 :conclusion [(= t t)]
 :forward   true} 
{:name "equal-e"
 :prereq [(substitutable? phi _:x a)
          (seq? phi)]
 :given  [(= a b) phi _:x]
 :conclusion [(substitution phi _:x b)]
 :forward true}
;; sometimes you need this
{:name "equal-reflexivity"
 :given      [(= a b)]
 :conclusion [(= b a)]
 :forward   true
 :backward  true}

; FORALL 
{:name "forall-i"
 :given      [(infer (actual x0)
                     (substitution phi x x0))]
 :conclusion [(forall [x] phi)]
 :backward  true}
{:name "forall-e"
 :given      [(forall [x] phi)
              (actual t)]
 :conclusion [(substitution phi x t)]
 :forward   true}

; EXISTS
{:name "exists-i"
 :given      [(actual t)
              (substitution phi x t)]
 :conclusion [(exists [x] phi)]
 :backward  true}
{:name "exists-e"
 :given      [(exists [x] phi)
              (infer [(actual x0)
                      (substitution phi x x0)]
                     X)]
 :conclusion [X]
 :backward  true}



