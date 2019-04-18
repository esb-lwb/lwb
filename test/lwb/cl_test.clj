(ns lwb.cl-test
  (:require [lwb.cl :refer :all]
            [clojure.test :refer :all]))

;; Syntax ----------------------------------------------------------------

;; combinator?

(deftest combinator?-test
  (is (= true  (combinator? 'S)))
  (is (= false (combinator? 'x)))
  (is (= true  (combinator? 'Phi)))
  (is (= false (combinator? false)))
  (is (= false (combinator? '[K x])))
  (is (= false (combinator? '[((S x) y)]))))

;; variable?

(deftest variable?-test
  (is (= true  (variable? 'y)))
  (is (= false (variable? 'S)))
  (is (= true  (variable? 'phi)))
  (is (= false (variable? false)))
  (is (= false (variable? '[K x])))
  (is (= false (variable? '[((S x) y)]))))

;; wff?

(deftest wff?-test
  (is (= true  (wff? '[S])))
  (is (= true  (wff? '[K])))
  (is (= true  (wff? '[(I x)])))
  (is (= true  (wff? '[(x (y z))])))
  (is (= true  (wff? '[(z S)])))
  (is (= true  (wff? '[((S I) I)])))
  (is (= false (wff? '((S I) I))))
  (is (= false (wff? '(S I I))))
  (is (= false (wff? '[(S)])))
  (is (= false (wff? '[(S (x))]))))

;; variables
(deftest variables-test
  (is (= '#{x y z} (variables '[K x (W y z) y])))
  (is (= '#{x} (variables '[x x x])))
  (is (= '#{y} (variables '[(S I) y B])))
  (is (= '#{x_14 x_152} (variables '[x_14 x_152])))
  (is (= '#{x y z} (variables '[M_1 (M_2 (M_3 x y y) z)]))))

;; combinators

(deftest combinators-test
  (is (= '#{K W} (combinators '[K x (W y z) y])))
  (is (= '#{} (combinators '[x x x])))
  (is (= '#{S I B} (combinators '[(S I) y B])))
  (is (= '#{} (combinators '[x_14 x_152])))
  (is (= '#{M_1 M_2 M_3} (combinators '[M_1 (M_2 (M_3 x y y) z)]))))

;; vfree?

(deftest vfree?-test
  (is (= true  (vfree? 'w '(((K x) ((W y) z)) y))))
  (is (= false (vfree? 'x '(((K x) ((W y) z)) y)))))

;; size
(deftest size-test
  (is (= 6 (size '[K x (W y z) y])))
  (is (= 3 (size '[x x x])))
  (is (= 4 (size '[(S I) y B])))
  (is (= 2 (size '[x_14 x_152])))
  (is (= 7  (size '[M_1 (M_2 (M_3 x y y) z)]))))

;; Parentheses -----------------------------------------------------------

;; max-parens
(deftest max-parens-test
  (is (= '[(((B x) y) z)] (max-parens '[(B x y) z])))
  (is (= '[((((B W) B) x) (((B W) B)x))] (max-parens '[B W B x (B W B x)])))
  (is (= '[(((B x) y)z )] (max-parens '[B x y z])))
  (is (= '[(((K x)((W y) z)) y)] (max-parens '[K x (W y z) y])))
  (is (= '[((x x) x)] (max-parens '[x x x])))
  (is (= '[(((S I) y) B)] (max-parens '[(S I) y B])))
  (is (= '[(x_14 x_152)] (max-parens '[x_14 x_152])))
  (is (= '[(M_1 ((M_2 (((M_3 x) y) y)) z))] (max-parens '[M_1 (M_2 (M_3 x y y) z)]))))

;; min-parens

(deftest min-parens-test
  (is (= '[y z (I I) x] (min-parens '[(((y z) (I I)) x)])))
  (is (= '[M x (B y (W z))] (min-parens '[((M x) ((B y) (W z)))])))
  (is (= '[W (I x_145 (I x_72)) x_58] (min-parens '[(W (I x_145 (I x_72)) x_58)])))
  (is (= '[S K K x] (min-parens '[(((S K) K) x)])))
  (is (= '[S M M (I (N P))] (min-parens '[(((S M) M) (I (N P)))]))))

(run-tests)
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

