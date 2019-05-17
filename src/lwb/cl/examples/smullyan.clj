; lwb Logic WorkBench -- Combinatory logic, Examples from Raymond Smullyan: How to Mock a Mockingbird

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.examples.smullyan
  (:require [lwb.cl :refer :all]
            [lwb.cl.repl :refer :all]))

;; Chap 9: To Mock a Mockingbird ----------------------------------------------
(reset-combinators)

;; Mockingbird p.73
(def-combinator '[M x] '[x x] "Mockingbird")
(show-combinators)

;; Composition
;; Definition: C composes A with B if for every x: C x = A (B x).
;; The composition condition: For every A, B exists C that composes A with B.

;; Definition: A is fond of B if A B = B.

;; Definition: A is called egocentric if A A = A, i.e. A is fond of itself.

;; Definition: Two combinators A, B agree on x if A x = B x.
;; Definition: A is agreeable if for every B there is a x such that A x = B x.

;; Definition: Two combinators A and B are compatible if there are x, y such that A x = y and B y = x.
;; Definition: A combinator A is happy if there are x, y such that A x = y and A y = x, 
;;             i.e. A is compatible with itself.

;; Definition: A combinator A is normal if exists B such that A B = B, i.e. exists B such that A is fond of B.

;; Definition: A combinator B is hopelessly egocentric if B B = B.
;; Definition: A is fixated on B if for every x: A x = B.

;; Kestrel p.77
(def-combinator '[K x y] '[x] "Kestrel")
(show-combinators)
;; Left cancellation law for kestrels: if K x = K y then x = y.

;; Identity Bird aka Idiot p.78
(def-combinator '[I x] '[x] "Identity Bird")
(show-combinators)

;; Lark p.80
(def-combinator '[L x y] '[x (y y)] "Lark")
(show-combinators)

;; 24
;  Given L and I, then there is a M with M x = x x.
(session '[L I x])
(red :L)
(red :I)
; or
(weak-reduce '[L I x])
; L I x -> x x, i.e. M = L I
 
;; 25
; Given L, then every bird x is fond of at least one bird
(session '[x (L x (L x))])
(exp :L)
; x (L x (L x)) = L x (L x)
; Any bird x is fond of L x (L x) = (L x) (L x)

;; 26
; Given L, then there is a bird of whom L L is fond
(session '[L L ((L (L L)) (L (L L)))])
(red :L)
; => [L (L (L L) (L (L L)) (L (L L) (L (L L))))]   

;; Chap 10: Is There a Sage Bird? ---------------------------------------------

; A sage bird Theta is a bird with x (Theta x) = Theta x, aka a fixed point combinator

; There is such a bird: the composition of M and L
(session '[M (L x)])
(red :M)
(red :L)
(exp :M)

(session '[x (M (L x))])
(red :M)
(exp :L)
(exp :M)
; [x (M (L x))] = [M (L x)]

;; Chap 11: Birds Galore ------------------------------------------------------

; Introduction
(max-parens '[x y (z w y) v])
; => [(((x y) ((z w) y)) v)]
(max-parens '[(x y z) (w v x)])
; => [(((x y) z) ((w v) x))]
(max-parens '[x y (z w v) (x z)])
; => [(((x y) ((z w) v)) (x z))]
(max-parens '[x y (z w v) x z])
; => [((((x y) ((z w) v)) x) z)]
(max-parens '[x (y (z w v)) x z])
; => [(((x (y ((z w) v))) x) z)]
(= (max-parens '[x y z (A B)]) (max-parens '[(x y z) (A B)]))
; => true
(= (max-parens '[x y w]) (max-parens (subst '[z w] '[z] '(x y))))
; => true
(= (max-parens '[w x y]) (max-parens (subst '[w z] '[z] '(x y))))
; => false

;; Bluebird p.95
(def-combinator '[B x y z] '[x (y z)] "Bluebird")
(show-combinators)

;; 1
; Given B then the composition law holds
(session '[(B f g) x])
(red :B)
; => [f (g x)] i.e. [B f g] composes f with g.

; Let's use this fact
(session '[B M L x])
(red :B)
(red :M)
(red :L)
(exp :M)
(exp :B 2)
; => [x (B M L x) i.e. B M L is a sage bird

;; 2
; Given B and M, write down an expression in terms of B, M, x that describes a bird of which x is fond.
(session '[(M (B x M))])
(red :M)
(red :B)
; => [x (M (B x M))] 

(session '[x (M (B x M))])
(exp :B)
(exp :M)
; => [M (B x M)]  
; x is fond of M (B x M)

;; 3
; Given B and M, find egocentric bird
(session '[M (B M M)])
(red :M)
(red :B)
(red :M)
; => [M (B M M) (M (B M M))]  
; M (B M M) is egocentric, i.e. Z = Z Z

;; 4
; Given B, M and K, find hopelessly egocentric bird
; M (B K M) is hopelessly egocentric , i.e. for all x: Z x = Z
(session '[M (B K M) x])
(red :M)
(red :B)
(red :K)
; => [M (B K M)] 
 
(session '[M (B K M)])
(exp :K)
(swap '?1 '[x])
(exp :B)
(exp :M)
; => [M (B K M) x] 

;; Dove p.97
(def-combinator '[D x y z w] '[x y (z w)] "Dove")
(show-combinators)

;; 5
; D can be derived from B alone, how?
(session '[B B x y z w])
(red :B)
(red :B)
; => [x y (z w)], i.e. D = B B.
(= (weak-reduce '[B B x y z w]) (weak-reduce '[D x y z w]))
; => true

;; Blackbird p.97
(def-combinator '[B1 x y z w] '[x (y z w)] "Blackbird")
(show-combinators)

;; 6
; Given B, then there is a D and a B1 too:
; D = B B, and B1 = D B = B B B
(weak-reduce '[D B x y z w])
; => [x (y z w)], i.e. B1 = D B.

(weak-reduce '[B B B x y z w])
; => [x (y z w)], i.e. B1 = B B B.

;; Eagle p.97
(def-combinator '[E x y z w v] '[x y (z w v)] "Eagle")
(show-combinators)

;; 7
; Derive E from B
(weak-reduce '[B B1 x y z w v])
; => [x y (z w v)], i.e. E = B B1 = B (B B B).

;; Bunting p.97
(def-combinator '[B2 x y z w v] '[x (y z w v)] "Bunting")
(show-combinators)

;; 8
; Given B find B2
(weak-reduce '[E B x y z w v])
; => x (y z w v)]
(weak-reduce '[B B1 B x y z w v])
; => [x (y z w v)], i.e. B2 = B (B B B) B
; but this is true too
(weak-reduce '[B B B1 x y z w v])
; => [x (y z w v)], i.e. B2 = B B (B B B)

;; Dickcissel p.97
(def-combinator '[D1 x y z w v] '[x y z (w v)] "Dickcissel")
(show-combinators)

;; 9
; Derive D1 from B
(weak-reduce '[B1 B x y z w v])
; => [x y z (w v)]
(weak-reduce '[B B B B x y z w v])
; => [x y z (w v)], i.e. D1 = B B B B
(weak-reduce '[B (B B) x y z w v])
; => [x y z (w v)], i.e. D1 = B (B B)

;; Becard p.98
(def-combinator '[B3 x y z w] '[x (y (z w))] "Becard")
(show-combinators)

;; 10
; Derive B3 from B
(weak-reduce '[D1 B x y z w])
; => [x (y (z w))]
(weak-reduce '[B (B B) B x y z w])
; => [x (y (z w))]
(weak-reduce '[B B B B B x y z w])
; => [x (y (z w))]

;; Dovekie p.98
(def-combinator '[D2 x y z w v] '[x (y z) (w v)] "Dovekie")
(show-combinators)

;; 11
; Derive D2 from B
(weak-reduce '[B B (B B) x y z w v])
; => [x (y z) (w v)], i.e. D2 = B B (B B)
(weak-reduce '[D D x y z w v])
; => [x (y z) (w v)], i.e. D2 = D D

;; Bald Eagle p.98
(def-combinator '[Ê x y1 y2 y3 z1 z2 z3] '[x (y1 y2 y3) (z1 z2 z3)] "Bald Eagle")
(show-combinators)

;; 12
; Derive Ê from B
(weak-reduce '[E E  x y_1 y_2 y_3 z_1 z_2 z_3])
; => [x (y_1 y_2 y_3) (z_1 z_2 z_3)]
(weak-reduce '[B (B B B) (B (B B B)) x y_1 y_2 y_3 z_1 z_2 z_3])
; => [x (y_1 y_2 y_3) (z_1 z_2 z_3)], i.e. Ê = B (B B B) (B (B B B))

;; Warbler p.99
(def-combinator '[W x y] '[x y y] "Warbler")
(show-combinators)

;; 14
; Derive M from W and I
(weak-reduce '[W I x])
(weak-reduce '[M x])
; => M = W I

;; 15
; Derive I from W and K
(weak-reduce '[W K x])
; => W K = I

;; 13
; Now derive M from W and K
(weak-reduce '[W (W K) x])
; => [x x], i.e. W (W K) = M.
; in steps:
(session '[W (W K) x])
(red :W)
(red :W)
(red :K)

;; Cardinal p.100
(def-combinator '[C x y z] '[x z y] "Cardinal")
(show-combinators)

;; 16
; Derive I from C and K
(weak-reduce '[C K K x])
; => [x], i.e. C K K = I.
(weak-reduce '[C K C x])
; => [x]
(weak-reduce '[C K any x])
; => [x]

;; Trush p.100
(def-combinator '[T x y] '[y x] "Trush")
(show-combinators)

;; 17
; Derive T from C and I
(weak-reduce '[C I x y])
; => [y x], i.e. T = C I.

;; Definition: A and B commute if A B = B A

;; 19
; Given B, T and M find a bird that commutes with every bird
(session '[M (B T M) x])
(red :M)
(red :B)
(red :T)
; => [x (M (B T M))], i.e. M (B T M) commutes with any x.

;; Robin p.101
(def-combinator '[R x y z] '[y z x] "Robin")
(show-combinators)

;; 20
; Derive R from B and T
(weak-reduce '[B B T x y z])
; => y z x], i.e. R = B B T.

;; 21
; Derive C from R alone
(weak-reduce '[R R R x y z])
; => [x z y], i.e. R R R = C
; Derive C from B B T with 9 letters
(weak-reduce '[B B T (B B T) (B B T) x y z])
; => [x z y] 
; A shorter term with 8 letters?
(weak-reduce '[B B T (B B T) (B B T)])
; => [B (T (B B T)) (B B T)] hence
(weak-reduce '[B (T (B B T)) (B B T) x y z])
; => [x z y]

;; 22 Two useful laws
;; 22a
(session '[C x y z])
(red :C)
(exp :R)
(exp :R 2)
; => [R x R y z], i.e. C x = R x R.

;; 22b
(session '[C x y z])
(red :C)
(exp :R)
(exp :T 2)
(exp :B)
; => [B (T x) R y z], i.e. C x = B (T x) R. 

;; 23
; Derive R from C
(weak-reduce '[C C x y z])
; => [y z x]
(= (weak-reduce '[C C x y z]) (weak-reduce '[R x y z]))
; => true

;; Finch p.102
(def-combinator '[F x y z] '[z y x] "Finch")

;; 24
(weak-reduce '[B C R x y z])
; => [z y x]
(weak-reduce '[B (R R R) R x y z])
; => [z y x]
(weak-reduce '[B C (C C) x y z])
; => [z y x]

;; 25
(weak-reduce '[E T T E T x y z])
; => [z y x]

;; 26
(weak-reduce '[B C R x y z])
(weak-reduce '[B (R R R) R x y z])
(weak-reduce '[B (B (T (B B T)) (B B T)) (B B T) x y z])
; => [z y x]

(weak-reduce '[E T T E T x y z])
(weak-reduce '[B (B B B) T T (B (B B B)) T x y z])
; => [z y x]
(weak-reduce '[B (B B B) T T (B (B B B)) T])
; => [B (T T) (B (B B B) T)], thus
(weak-reduce '[B (T T) (B (B B B) T) x y z] )
; => [z y x] a bit shorter than the solution above

;; Vireo p.103
(def-combinator '[V x y z] '[z x y] "Vireo")

;; 27
(weak-reduce '[C F x y z])
; => [z x y], i.e. V = C F
(weak-reduce '[B C T x y z])
; => [z x y], i.e. V = B C T

;; 28
(weak-reduce '[R R R F x y z])
; => [z x y], and also
(weak-reduce '[R F R x y z])
; => [z x y]

;; 29
(weak-reduce '[C V x y z])
; => [z y x], i.e. C V = F

;; 30
(weak-reduce '[R K K x])
; => [x]
(weak-reduce '[R R K x])
; => [x]

;; Cardinal once removed p.104
(def-combinator '[C* x y z w] '[x y w z] "Cardinal once removed")

;; 31
(weak-reduce '[B C x y z w])
; => [x y w z], i.e. C* = B C

;; Robin once removed p.104
(def-combinator '[R* x y z w] '[x z w y] "Robin once removed")

;; 32
(weak-reduce '[B C (B C) x y z w])
; => [x z w y], i.e. R* = B C (B C) = C* C*

;; Finch once removed p.104
(def-combinator '[F* x y z w] '[x w z y] "Finch once removed")

;; 33
(weak-reduce '[B C* R* x y z w])
(weak-reduce '[B C* (C* C*) x y z w])
; => [x w z y], i.e. F* = B C* R* = B C* (C* C*)

;; Vireo once removed p.104
(def-combinator '[V* x y z w] '[x w y z] "Vireo once removed")

;; 34
(= (weak-reduce '[C* F* x y z w]) (weak-reduce '[V* x y z w]))
; => true

;; Cardinal twice removed p.105
(def-combinator '[C** x y z_1 z_2 z_3] '[x y z_1 z_3 z_2] "Cardinal twice removed")
;; Robin twice removed p.105
(def-combinator '[R** x y z_1 z_2 z_3] '[x y z_2 z_3 z_1] "Robin twice removed")
;; Finch twice removed p.105
(def-combinator '[F** x y z_1 z_2 z_3] '[x y z_3 z_2 z_1] "Finch twice removed")
;; Vireo twice removed p.105
(def-combinator '[V** x y z_1 z_2 z_3] '[x y z_3 z_1 z_2] "Vireo twice removed")

;; 35
(= (weak-reduce '[C** x y z_1 z_2 z_3]) (weak-reduce '[B C* x y z_1 z_2 z_3]))
; => true, i.e. C** = B C*
(= (weak-reduce '[R** x y z_1 z_2 z_3]) (weak-reduce '[B R* x y z_1 z_2 z_3]))
; => true, i.e. R** = B R*
(= (weak-reduce '[F** x y z_1 z_2 z_3]) (weak-reduce '[B F* x y z_1 z_2 z_3]))
; => true, i.e. F** = B F*
(= (weak-reduce '[V** x y z_1 z_2 z_3]) (weak-reduce '[B V* x y z_1 z_2 z_3]))
; => true, i.e. V** = B V*

;; 36
(= (weak-reduce '[V x y z]) (weak-reduce '[C* T x y z]))
; => true, i.e. V = C* T

;; Queer p.105
(def-combinator '[Q x y z] '[y (x z)] "Queer")

;; 39
(= (weak-reduce '[Q x y z]) (weak-reduce '[C B x y z]))
; => true, i.e. Q = C B
(= (weak-reduce '[Q x y z]) (weak-reduce '[B (T B) (B B T) x y z]))
; => true, i.e. Q = B (T B) (B B T)

;; Quixotic p.106
(def-combinator '[Q1 x y z] '[x (z y)] "Quixotic")

;; 38
(= (weak-reduce '[Q1 x y z]) (weak-reduce '[B C B x y z]))
; => true i.e. Q1 = B C B
(= (weak-reduce '[Q1 x y z]) (weak-reduce '[C* B x y z]))
; => true i.e. Q1 = C* B
(= (weak-reduce '[Q1 x y z]) (weak-reduce '[B (B B T (B B T) (B B T)) B x y z]))
; => true

;; Quizzical p.106
(def-combinator '[Q2 x y z] '[y (z x)] "Quizzical")

;; 39
(= (weak-reduce '[Q2 x y z]) (weak-reduce '[C (B C B) x y z]))
; => true, i.e. Q2 = C (B C B)

;; Quirky p.106
(def-combinator '[Q3 x y z] '[z (x y)] "Quirky")

;; 41
(= (weak-reduce '[Q3 x y z]) (weak-reduce '[B T x y z]))
; => true, i.e. Q3 = B T
(= (weak-reduce '[Q3 x y z]) (weak-reduce '[V* B x y z]))
; => true, i.e. Q3 =  V* B

;; Quacky p.106
(def-combinator '[Q4 x y z] '[z (y x)] "Quacky")

;; 44
(= (weak-reduce '[Q4 x y z]) (weak-reduce '[Q1 T x y z]))
; => true, i.e. Q4 = Q1 T
(= (weak-reduce '[Q4 x y z]) (weak-reduce '[B C B T x y z]))
; => true, i.e. Q4 = B C B T
(= (weak-reduce '[Q4 x y z]) (weak-reduce '[C Q3 x y z]))
; => true, i.e. Q4 = C Q3

;; 45
(= (weak-reduce '[B x y z]) (weak-reduce '[Q T (Q Q) x y z]))
; => true, i.e. B = Q T (Q Q)

;; 46
(= (weak-reduce '[C x y z]) (weak-reduce '[Q Q (Q T) x y z]))
; => true, i.e. C = Q Q (Q T)

;; Goldfinch p.107
(def-combinator '[G x y z w] '[x w (y z)] "Goldfinch")

;; 47
(= (weak-reduce '[G x y z w]) (weak-reduce '[B B C x y z w]))
; => true, i.e. G = B B C

;; Chap 12: Mockingbirds, Warblers and Starlings ------------------------------

;; Double Mockingbird p.118
(def-combinator '[M2 x y] '[x y (x y)] "Double Mockingbird")

;; 1
(= (weak-reduce '[M2 x y]) (weak-reduce '[B M x y]))
; => true, i.e. M2 = B M

;; 2
(= (weak-reduce '[L x y]) (weak-reduce '[C B M x y]))
; => true, i.e. L = C B M
(= (weak-reduce '[L x y] (weak-reduce '[B (T M) B x y])))
; => true, i.e. L = B (T M) B

;; 3
(= (weak-reduce '[L x y]) (weak-reduce '[B W B x y]))
; => true, i.e. L = B W B

;; 4
(= (weak-reduce '[L x y]) (weak-reduce '[Q M x y]))
; => true, i.e. L = Q M 

;; Converse Warbler p.119
(def-combinator '[W' x y] '[y x x] "Converse Warbler")

;; 5
(= (weak-reduce '[W' x y]) (weak-reduce '[C W x y]))
; => true, i.e. W' = C W 
(= (weak-reduce '[W' x y]) (weak-reduce '[B M R x y]))
; => true, i.e. W' = B M R
(= (weak-reduce '[W' x y]) (weak-reduce '[B M (B B T) x y]))
; => true, i.e. W' = B M (B B T)
(= (weak-reduce '[W' x y]) (weak-reduce '[B (B M B) T x y]))
; => true, i.e. W' = B (B M B) T

;; 6
(= (weak-reduce '[W x y]) (weak-reduce '[C W' x y]))
; => true, i.e. W = C W' 
(= (weak-reduce '[W x y]) (weak-reduce '[C (B M R) x y]))
; => true, i.e. W = C (B M R) 

;; 7
(= (weak-reduce '[W x y]) (weak-reduce '[B (T (B M (B B T))) (B B T) x y]))
; => true
(= (weak-reduce '[W x y]) (weak-reduce '[B (T (B (B M B)T)) (B B T) x y]))
; => true

;; 8
(= (weak-reduce '[M x]) (weak-reduce '[W T x]))
; => true, i.e. M = W T

;; Warbler Once Removed p.120
(def-combinator '[W* x y z] '[x y z z] "Warbler once removed")

;; Warbler Twice Removed p.120
(def-combinator '[W** x y z w] '[x y z w w] "Warbler twice removed")

;; 9
(= (weak-reduce '[W* x y z]) (weak-reduce '[B W x y z]))
; => true, i.e. W* = B W
(= (weak-reduce '[W** x y z w]) (weak-reduce '[B (B W) x y z w]))
; => true, i.e. W** = B (B W)

;; Hummingbird p.120
(def-combinator '[H x y z] '[x y z y] "Hummingbird")

;; 10
(= (weak-reduce '[H x y z]) (weak-reduce '[B W (B C) x y z]))
; => true, i.e. H = B W (B C)

;; 11
(= (weak-reduce '[W x y z]) (weak-reduce '[C (H R) x y z]))
; => true, i.e. W = C (H R)

;; Starling p.121
(def-combinator '[S x y z] '[x z (y z)] "Starling")

;; 12
; S from B C W
(= (weak-reduce '[S x y z]) (weak-reduce '[B (B W) (B B C) x y z]))
; => true, i.e. S = B (B W) (B B C)
; S from B T M ?? brute force!!
(def S-red1 (subst '[B (B W) (B B C)] '[W] '[B (T (B M (B B T))) (B B T)]))
(weak-reduce (comb-concat S-red1 '[x] '[y] '[z]))
; => [x z (y z)]
(def S-red2 (subst S-red1 '[C] '[B (T (B B T)) (B B T)]))
(weak-reduce (comb-concat S-red2 '[x] '[y] '[z]))
S-red2
; => [B (B (B (T (B M (B B T))) (B B T))) (B B (B (T (B B T)) (B B T)))]
; S from B W G
(= (weak-reduce '[S x y z]) (weak-reduce '[B (B W) G x y z]))
; => true, i.e. S = B (B W) G

;; 13
(= (weak-reduce '[H x y z]) (weak-reduce '[S (C C) x y z]))
; => true, i.e. H = S (C C)
(= (weak-reduce '[H x y z]) (weak-reduce '[S R x y z]))
; => true, i.e. H = S R

;; 14
(= (weak-reduce '[W x y z]) (weak-reduce '[R (S R R) R x y z]))
; => true, i.e. W =  R (S R R) R
(= (weak-reduce '[W x y z]) (weak-reduce '[C (S (C C) (C C)) x y z]))
; => true, i.e. W =  C (S (C C) (C C))

;; 15
(= (weak-reduce '[W x y z]) (weak-reduce '[S T x y z]))
; => true, i.e. W = S T

;; 16
(= (weak-reduce '[M x]) (weak-reduce '[S T T x]))
; => true, i.e. M = S T T

;; Bases:
;; Church B, T, M, I
;; Curry  B, C, W, I
;; alt    B, C, S, I

;; G1 p.124 G1 = B (B B C)
(def-combinator '[G1 x y z w v] '[x y v (z w)] "G1")

; G1 from B T
(= (weak-reduce '[G1 x y z w v]) (weak-reduce '[B (B B C) x y z w v]))
(= (weak-reduce '[G1 x y z w v]) (weak-reduce '[B G x y z w v]))
(= (weak-reduce '[G1 x y z w v]) (weak-reduce '[B (B B (B (T (B B T)) (B B T))) x y z w v]))
; => all true

;; G2 p.124 G2 = G1 (B M)
(def-combinator '[G2 x y z w] '[x w (x w) (y z)] "G2")

; G2 from G1 M
(= (weak-reduce '[G2 x y z w]) (weak-reduce '[G1 (B M) x y z w]))
; => true

;; I2 p.124 I2 = B (T I) (T I)
(def-combinator '[I2 x] '[x I I] "I2")

; I2 from B T I
(= (weak-reduce '[I2 x] (weak-reduce '[B (T I) (T I) x])))
; => true

; I2 (F x) = x
(weak-reduce '[I2 (F x)])
; => x

; G2 F (Q I2) = W
(= (weak-reduce '[G2 F (Q I2) x y z] (weak-reduce '[W x y z])))

; S = B (B (B W) C) (B B)
(= (weak-reduce '[S x y z] (weak-reduce '[B (B (B W) C) (B B) x y z])))

;; Phoenix p. 124 Phi = B (B S) B
(def-combinator '[Phi x y z w] '[x (y w) (z w)] "Phoenix")

; Phi from S B
(= (weak-reduce '[Phi x y z w] (weak-reduce '[B (B S) B x y z w])))

;; Psi p.124 Psi = B H (B B (B B))
(def-combinator '[Psi x y z w] '[x (y z) (y w)] "Psi")

; Psi from B H
(= (weak-reduce '[Psi x y z w] (weak-reduce '[B H (B B (B B)) x y z w])))

;; H* p. 124 H* = B H
(def-combinator '[H* x y z w] '[x y z w z] "Hummingbird once removed")
(weak-reduce '[B H x y z w])

; Psi from B C W
(= (weak-reduce '[Psi x y z w] (weak-reduce '[H* D2 x y z w])))
(= (weak-reduce '[Psi x y z w] (weak-reduce '[B H D2 x y z w])))
(= (weak-reduce '[Psi x y z w] (weak-reduce '[B (B W (B C)) (B B (B B)) x y z w])))

; Phi from B Psi K
(= (weak-reduce '[Psi x y z w] (weak-reduce '[Phi (Phi (Phi B)) B (K K) x y z w])))

;; Gamma p.124 Gamma = Phi (Phi (Phi B)) B
(def-combinator '[Gamma x y z w v] '[y (z w)  (x y w v)] "Gamma")

(= (weak-reduce '[Gamma x y z w v] (weak-reduce '[Phi (Phi (Phi B)) B x y z w v])))

; Psi from Gamma and K
(= (weak-reduce '[Psi x y z w] (weak-reduce '[Gamma (K K)  x y z w v])))
; => true, i.e. Psi = Gamma (K K)

;; S' p.125 S' = C S
(def-combinator '[S' x y z] '[y z (x z)] "S'")

(= (weak-reduce '[S' x y z] (weak-reduce '[C S x y z])))

(= (weak-reduce '[S' I x y z] (weak-reduce '[W x y z])))
; => true, i.e. W = S' I

; Q^ such that C Q^ W = S
;; Q^ = Q (Q Q (Q Q)) Q
(weak-reduce '[C (Q (Q Q (Q Q)) Q) W x y z])
; => [x z (y z)]

;; Chap 13: A Gallery of Sage Birds -------------------------------------------

;; Order of a combinator
;; The order of a combinator is the number of variables that are needed to express the combinator.
;; Examples:
;; Order 1: I and M
;; Order 2: T, L, W
;; Order 3: S, B, C, R, F ...
;; Order 7: Ê

;; Proper (and improper) combinators
;; A combinator having some order is called proper
;; T I is not proper: T I x -> x I, T I x y -> x I y we get not rid of I, so T I has no order
;; I T is proper since I T = T, and T is proper

;; Sage birds
;; A sage bird is a bird Theta such that for all x: x (Theta x) = Theta x
;; Sage birds are improper combinators, but they can be expressed in terms of proper
;; combinators.

;; Since Sage birds are often called Y

;; 1 A sage bird from M, B and R 
(show-combinator :M)
(show-combinator :B)
(show-combinator :R)

(session '[B M (R M B) x])
(red :B)
(red :M)
(red :R)
(red :B)
(red :M)
(exp :B 2)
(red :B)
(exp :M)
(exp :B 2)
; => [x (B M (R M B) x)]   

;; 2 A sage bird from B, C and M
(show-combinator :C)

(session '[B M (C B M) x])
(red :B)
(red :M)
(red :C)
(red :B)
(red :M)
(exp :C 2)
(exp :C 4)
(exp :M )
(exp :B 2)
; => [x (B M (C B M) x)] 

;; 3 A sage bird from M, B and L
(show-combinator :L)

(session '[B M L x])
(red :B)
(red :M)
(red :L)
(exp :M)
(exp :B 2)
; => [x (B M L x)] 

;; 4 A sage bird from M, B and W
(show-combinator :W)

(session '[B M (B W B) x]) 
(red :B)
(red :M)
(red :B)
(red :W)
(red :B)
(exp :M)
(exp :B 2)
; => [x (B M (B W B) x)]    

;; 6  A sage bird from Q, L, W
(show-combinator :Q)

(session '[W (Q L (Q L)) x])
(red :W)
(red :Q)
(red :Q)
(red :L)
(exp :Q 2)
(exp :Q 2)
(exp :W)
; => [x (W (Q L (Q L)) x)] 

;; 5, 7 A sage bird from B, C and W
(show-combinator :C)

(session '[W (B (C B (B W B)) (B W B)) x])
(red :W)
(red :B)
(red :C)
(red :B)
(red :B)
(red :W)
(red :B)
(exp :B 2)
(exp :C 2)
(exp :B 2)
(exp :W)
; => [x (W (B (C B (B W B)) (B W B)) x)] 

;; 8 A sage bird from Q and M

(session '[Q (Q M) M x])
(red :Q)
(red :M)
(red :Q)
(exp :Q 2)
; => [x (Q (Q M) M x)] 

;; 9 A sage bird from S and L

(session '[S L L x])
(red :S)
(red :L)
(exp :S)
; => [x (S L L x)]  

;; 10 A sage bird from B, W and S - Curry's sage bird

(session '[W S (B W B) x])
(red :W)
(red :S)
(red :B)
(red :W)
(red :B)
(exp :S)
(exp :W)
; => [x (W S (B W B) x)]    

;; Turing Bird U p.132  U = L (S (C T))
(def-combinator '[U x y] '[y (x x y)] "Turing Bird")

(weak-reduce '[L (S (C T)) x y])
; => [y (x x y)]

;; 11 A Turing bird from B, M and T
(show-combinator :T)

; W, L, Q are derivable from B, M and T
(weak-reduce '[B W (L Q) x y])
; => y (x x y)]

;; 12 A sage bird from  U
(session '[U U x])
(red :U)
; => x (U U x)], i.e. U U is a sage bird

;; Owl O p.133  U = B W (C B)
(def-combinator '[O x y] '[y (x y)] "Owl")

;; 13 O form B, C, W
(weak-reduce '[B W (C B) x y])
; => [y (x y)]

;; 14 U from O and L
(= (weak-reduce '[L O x y]) (weak-reduce '[U x y]))
; => U = L O

;; 15 M from O and I
(= (weak-reduce '[M x]) (weak-reduce '[O I x]))
; => M = O I

;; 16 O from S and I
(= (weak-reduce '[O x y]) (weak-reduce '[S I x y]))
; => O = S I

;; Two combinators A1 and A2 are similar if for all x: A1 x = A2 x

;; A theory of combinators is called extensional if similar birds are identical.
;; I.e. there is exactly on identity combinator, one starling and so forth.

;; Chap 19: Aristocratic Birds  -----------------------------------------------

; Jaybird p.181 J = [B (B C) (W (B C (B (B B B))))]
(def-combinator '[J x y z w] '[x y (x w z)] "Jaybird")

;; 1 J from B C W
(weak-reduce '[B (B C) (W (B C (B (B B B)))) x y z w])
; => x y (x w z)]

;; 2 Q1 from J and I
(= (weak-reduce '[Q1 x y z]) (weak-reduce '[J I x y z]))
; => Q1 = J I

;; 3 T from Q1 and I
(= (weak-reduce '[T x y]) (weak-reduce '[Q1 I x y]))
; => T = Q1 I = J I I

;; 4 R from J and T
(= (weak-reduce '[R x y z]) (weak-reduce '[J T x y z]))
; => R = J T

;; 5 B from C and Q1
(= (weak-reduce '[B x y z]) (weak-reduce '[C (Q1 C) Q1 x y z]))
; => B = C (Q1 C) Q1
(= (weak-reduce '[B x y z]) (weak-reduce '[C (J I C) (J I) x y z]))
; => B = C (J I C) (J I)

; J1 p.183 J1 = B J T
(def-combinator '[J1 x y z w] '[y x (w x z)] "J1")

;; 6 J1 from J, B and T
(weak-reduce '[B J T x y z w])
; => [y x (w x z)]

;; 7 M from C, T, J1
(= (weak-reduce '[M x]) (weak-reduce '[C (C (C J1 T) T) T x]))
; => M = C (C (C J1 T) T) T

;; Chap 20: Craig's Discovery -------------------------------------------------

;; 1 Q3 from G and I
(= (weak-reduce '[Q3 x y z]) (weak-reduce '[G I x y z]))
; => Q3 = G I

;; 2 T from G and I
(= (weak-reduce '[T x y]) (weak-reduce '[G I I x y]))
;=> T = G I I

;; 3 C from G and I
(= (weak-reduce '[C x y z]) (weak-reduce '[G G I I x y z]))
; => C = G G I I

;; 4 B from C Q
(= (weak-reduce '[R x y z]) (weak-reduce '[C C x y z]))
; => R = C C
(= (weak-reduce '[Q x y z]) (weak-reduce '[G R Q3 x y z]))
; => Q = G R Q3
(= (weak-reduce '[B x y z]) (weak-reduce '[C Q x y z]))
; => B = C Q
