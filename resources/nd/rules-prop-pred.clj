; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 - 2016 Tobias Völzel, Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

; AND
{:id :and-i
 :given      [a b]
 :conclusion [(and a b)]
 :forward   true
 :backward  true}
{:id :and-e1
 :given      [(and a b)]
 :conclusion [a]
 :forward   true}
{:id :and-e2
 :given      [(and a b)]
 :conclusion [b]
 :forward   true}

; OR
{:id :or-i1
 :given      [a]
 :conclusion [(or a b)]
 :forward   true
 :backward  true}
{:id :or-i2
 :given      [a]
 :conclusion [(or b a)]
 :forward   true
 :backward  true}
{:id :or-e
 :given      [(or a b)
              (infer a X)
              (infer b X)]
 :conclusion [X]
 :backward  true}

; IMPL
{:id :impl-i
 :given      [(infer a b)]
 :conclusion [(impl a b)]
 :backward   true}
{:id :impl-e
 :given      [a (impl a b)]
 :conclusion [b]
 :forward   true
 :backward  true}

; NOT
{:id :not-i
 :given      [(infer a contradiction)]
 :conclusion [(not a)]
 :backward   true}
{:id :not-e
 :given      [a (not a)]
 :conclusion [contradiction]
 :forward   true
 :backward  true}

; RAA/CONTRA
{:id :raa
 :given      [(infer (not a) contradiction)]
 :conclusion [a]
 :backward  true}
{:id :efq
 :given      [contradiction]
 :conclusion [a]
 :forward   true
 :backward  true}
          
; EQUAL
{:id :equal-i
 :given      []
 :conclusion [(= t t)]
 :forward   true} 
{:id :equal-e
 :prereq [(substitutable? phi _:x a)
          (seq? phi)]
 :given  [(= a b) phi _:x]
 :conclusion [(substitution phi _:x b)]
 :forward true}
;; sometimes you need this
{:id :equal-reflexivity
 :given      [(= a b)]
 :conclusion [(= b a)]
 :forward   true
 :backward  true}

; FORALL 
{:id :forall-i
 :given      [(infer (actual x0)
                     (substitution phi x x0))]
 :conclusion [(forall [x] phi)]
 :backward  true}
{:id :forall-e
 :given      [(forall [x] phi)
              (actual t)]
 :conclusion [(substitution phi x t)]
 :forward   true}

; EXISTS
{:id :exists-i
 :given      [(actual t)
              (substitution phi x t)]
 :conclusion [(exists [x] phi)]
 :backward  true}
{:id :exists-e
 :given      [(exists [x] phi)
              (infer [(actual x0)
                      (substitution phi x x0)]
                     X)]
 :conclusion [X]
 :backward  true}



