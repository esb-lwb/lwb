(ns lwb.cl-test
  (:require [lwb.cl :refer :all]
            [clojure.test :refer :all]))

;; Syntax ----------------------------------------------------------------

;; combinator?

;; variable?

;; wff?

;; variables

;; combinators

;; vfree?

;; size

;; Parentheses -----------------------------------------------------------

;; max-parens

;; min-parens

;; Subterms and substitution ------------------------------------------

;; subterms

;; subst

(comment
  (subst '[B f g x] '[B] '[S (K S) K])
  (subst '[B f g x] '[S (K S) K] '[B])
  )


;; Concatenation of terms ------------------------------------------------

;; comb-concat

;; Definition ------------------------------------------------------------

;; def-combinator

;; comb-defined?

;; show-combinator

;; show-combinators

;; reset-combinators

;; def-combinators-ski

;; def-combinatory-birds

;; One-step reduction and expansion -----------------------------------

;; one-step-red

;; one-step-exp

;; Weak reduction -----------------------------------------------------

;; weak-reduce

; interactive testing
(comment
  (def-combinatory-birds)
  
  (weak-reduce '[S (S x y z)(S x y z) z]) 
  ; => [x z (y z) z (x z (y z) z)]
  (weak-reduce '[S (S x y z)(S x y z) z] {:trace true}) 
  ; 3 steps
  
  ; The term that has no normalform
  (weak-reduce '[S I I (S I I)] {:trace true :cycle true})
  ; => [S I I (S I I)] in 3 steps
  (:overrun (meta (weak-reduce '[S I I (S I I)] {:trace true})))
  ; => true
  (weak-reduce '[S I I (S I I)] {:limit 2000 :timeout 200})
  ; => Timeout exception
  
  (weak-reduce '[Y f] {:limit 10 :trace true})
  ; => [f (f (f (f (f (f (f (f (f (f (Y f))))))))))] 10 steps
  )

;; weak-reduce-multi

; interactive testing
(comment
  (def-combinatory-birds)
  
  (weak-reduce-multi '[S (S x y z)(S x y z) z])
  ; => [x z (y z) z (x z (y z) z)]
  (weak-reduce-multi '[S (S x y z)(S x y z) z] {:trace true})
  ; 1 step

  ; The term that has no normalform
  (weak-reduce-multi '[S I I (S I I)] {:trace true :cycle true})
  ; => [S I I (S I I)] in 2 steps
  (meta (weak-reduce-multi '[S I I (S I I)] {:trace true :cycle true}))
  (:overrun (meta (weak-reduce-multi '[S I I (S I I)])))
  (:overrun (meta (weak-reduce-multi '[S I I (S I I)] {:trace true})))
  ; Timeout Exception
  ; TODO: but Threads goes on!!
  (:overrun (meta (weak-reduce-multi '[S I I (S I I)] {:limit 25 :timeout 0})))
  ; => true
  (weak-reduce-multi '[S I I (S I I)] {:limit 100000 :timeout 1000})

  (weak-reduce '[Y f] {:limit 10 :trace true})
  (meta (weak-reduce '[Y f] {:limit 10 :trace true :cycle true}))
  ; => [f (f (f (f (f (f (f (f (f (f (Y f))))))))))] 10 steps
  (weak-reduce-multi '[Y f])
  ; => timeout
  )

;; Bracket abstraction ------------------------------------------------

;; abstract
