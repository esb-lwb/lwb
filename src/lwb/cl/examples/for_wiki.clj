; lwb Logic WorkBench -- Examples for the wiki pages 

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.examples.for-wiki
  (:require [lwb.cl :refer :all]
            [lwb.cl.repl :refer :all]))

;; Syntax

(combinator? 'S)
; => true
(combinator? 'x)
; => false
(combinator? :S)
; => false
(combinator? 'Phi)
; => true

(variable? 'x)
; => true
(variable? 'S)
; => false

(wff? '[K x y])
; => true
(wff? '[(I x)])
; => true
(wff? '[x])
; => true
(wff? '[(x y)])
; => true
(wff? '[(x)])
; => false
(wff? '[(x)] :exception-if-not)
; Syntax error.
; (x) - failed: symbol? in: [0] at: [:simpl-expr :combinator] spec: :lwb.cl.spec/combinator
; (x) - failed: symbol? in: [0] at: [:simpl-expr :variable] spec: :lwb.cl.spec/variable
; (x) - failed: (> (count %) 1) in: [0] at: [:compl-expr] spec: :lwb.cl.spec/compl-expr

(combinators '[S B (K I) f x])
; => #{S K I B}
(variables '[S B (K I) f x])
; => #{x f}

(vfree? 'w '(((K x) ((W y) z)) y))
; => true
(vfree? 'x '(((K x) ((W y) z)) y))
; => false

(size '[K x (W y z) y])
; => 6
(size '[x x x])
; => 3
(size '[(S I) y B])
; => 4

; Parentheses

(max-parens '[(B x y) z])
; => [(((B x) y) z)]
(max-parens '[B W B x (B W B x)])
; => [((((B W) B) x) (((B W) B) x))]
(max-parens '[B x y z])
; => [(((B x) y) z)]
(max-parens '[K x (W y z) y])
; => [(((K x) ((W y) z)) y)]

(min-parens '[(((y z) (I I)) x)])
; => [y z (I I) x]
(min-parens '[((M x) ((B y) (W z)))])
; => [M x (B y (W z))]
(min-parens '[(((S K) K) x)])
; => [S K K x]
(min-parens '[(((S M) M) (I (N P)))])
; => [S M M (I (N P))]

; subterms and substitution

(subterms '[(((S M) M) (I (N P)))])
; => ([S M M (I (N P))] [S M M] [S M] [S] [M] [I (N P)] [I] [N P] [N] [P])

(subst '[S K K x y] '[S K K] '[I])
; => [I x y]
(subst '[I (x y) z] '[I] '[S K K])
; => [S K K (x y) z]
(subst '[I (x y) I z] '[I] '[S K K])
; => [S K K (x y) (S K K) z]

(comb-concat '[S] '[K] '[K])
; => [S K K]
(comb-concat '[S B (K I)] '[f] '[x])
; => [S B (K I) f x]
(comb-concat '[B] '[S B (K I)] '[S B (K I)])
; => [B (S B (K I)) (S B (K I))]

; Definition of combinators

(def-combinator '[S x y z] '[x z (y z)])
(def-combinator '[K x y] '[x])
(def-combinator '[I x] '[x])

(def-combinator '[x y z] '[x (y z)])
; Syntax error (AssertionError).
; Redex '[x y z]' has no combinator!

(comb-defined? :S)
; => true
(comb-defined? :Y)
; => false
(comb-defined? 'I)
; => false

(show-combinator :K)
; K := [K x y] -> [x] 
; => nil
(show-combinator :Y)
; No combinator with key ':Y' defined.
; => nil

(show-combinators)
; --- Defined combinators --------
; I  := [I x] -> [x]
; K  := [K x y] -> [x]
; S  := [S x y z] -> [x z (y z)]
; --------------------------------
; => nil

(reset-combinators)
; => {}
(show-combinators)
; There are no combinators defined yet!
; => nil

(def-combinators-ski)
(show-combinators)

(def-combinator '[I x] '[x] "Identitätsfunktion")
(def-combinator '[K x y] '[x] "Konstanzfunktion")
(def-combinator '[S x y z] '[x z (y z)] "Verschmelzungsfunktion")
(show-combinators)

(show-combinator :S)

; One-step reduction and expansion

(one-step-red '[S x y z] :S)
; => [x z (y z)]
(one-step-red '[S x y] :S)
; => [S x y]
(one-step-red '[S (S x y z) b c] :S)
; => [S x y z c (b c)]
(one-step-red '[S (S x y z) b c] :S 2)
; => [S (x z (y z)) b c]
(one-step-red '[S (x z (y z)) b c] :S 1)
; [x z (y z) c (b c)]

(one-step-exp '[x z (y z)] :S)
; => [S x y z]
(one-step-exp '[x z (y z) c (b c)] :S)
; => [S (x z (y z)) b c]
(one-step-exp '[x z (y z) c (b c)] :S 2)
; => [S x y z c (b c)] 

(one-step-exp '[x] :K)
; => [K x _0]

(one-step-exp '[x y] :K)
; => [K (x y) _0]
(one-step-exp '[x y] :K 3)
; => [x (K y _0)]


; Weak reduction

; example 1
(weak-reduce '[S (S x y z) (S x y z) z])
; => [x z (y z) z (x z (y z) z)]
(weak-reduce '[S (S x y z) (S x y z) z] {:trace true})
; 0: [S (S x y z) (S x y z) z]
; 1: [S x y z z (S x y z z)]
; 2: [x z (y z) z (S x y z z)]
; 3: [x z (y z) z (x z (y z) z)]
; => [x z (y z) z (x z (y z) z)]

; example 2
(weak-reduce '[S I I (S I I)])
(meta (weak-reduce '[S I I (S I I)]))
;{:reduced ((I (I (I (I ((S I) I))))) (I (I (I (I (I (I (I (I (I (I (I (I (I ((S I) I))))))))))))))),
; :no-steps 100,
; :cycle :unknown,
; :overrun true}

; weak-reduce always tries the first possible reduction
; there is another possibility for this term

(session '[S I I (S I I)])
(red :S)
(red :I)
(red :I)
; => [S I I (S I I)]; a cycle  

(weak-reduce '[S I I (S I I)] {:limit 3 :trace true})

; example 3
(def-combinator '[W x y] '[x y y])
(meta (weak-reduce '[W I (W I)]))
; => {:reduced ((W I) (W I)), :no-steps 100, :cycle :unknown, :overrun true}
(weak-reduce '[W I (W I)] {:cycle true})
; => [W I (W I)]
(weak-reduce '[W I (W I)] {:cycle true :trace true})
; 0: [W I (W I)]
; 1: [I (W I) (W I)]
; 2: [W I (W I)]
; => [W I (W I)]

; example 4
(def-combinator '[Y x] '[x (Y x)])

(weak-reduce '[Y x])
; => a very long expression
(weak-reduce '[Y x] {:limit 5})
; => [x (x (x (x (x (Y x)))))]
(weak-reduce '[Y x] {:timeout 1000 :limit 0})
; Syntax error (ExceptionInfo)
; Timeout after 1000 msecs.


; Bracket Abstraction
(abstract '[x] '[K x y])
; => [S K (K y)]

(weak-reduce '[S K (K y) P])
; => P

(weak-reduce (subst '[K x y] '[x] '[P]))
; => P

(abstract '[x y z] '[x z (y z)])
; => S
(abstract '[x y] '[y x])
; => [S (K (S I)) K]
(abstract '[x y] '[y x] curry-weak)
; => [S (K (S I)) (S (K K) I)]
(abstract '[x y] '[y x] curry-naive)
; => [S (S (K S) (K I)) (S (K K) I)]

(weak-reduce '[S (K (S I)) K a b])
; => [b a]

; Interactive Combinatory Logic
(session '[B M (R M B) x])

(def-combinator '[B f g x] '[f (g x)])
(def-combinator '[M x] '[x x])
(def-combinator '[R x y z] '[y z x])

(red :B)
(red :M)
(red :R)
(red :B)
(red :M)
(exp :B 2)
(red :B)
(exp :M)
(exp :B 2)


(def-combinators-ski)
(session '[x])
(exp :K)
(swap '?1 '[(K x)])
(exp :K)
(undo)
(exp :S)
