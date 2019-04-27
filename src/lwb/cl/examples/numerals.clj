; lwb Logic WorkBench -- Church encoding of natural numbers

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.examples.numerals
  (:require [lwb.cl :refer :all]))

(def-combinators-ski)
(def-combinator '[B a b c] '[a (b c)])


;; Definition of natural numbers ----------------------------------------------

;; We use the following encoding:
;; 0 = [K I]
;; 1 = [S B (K I)]
;; 2 = [S B (S B (K I))]
;; and so on

;; Zero
(def Zero '[K I])

;; Successor
(def Succ '[S B])

(defn succ
  "The successor of the given numeral.
   Pre: `term` is a numeral."
  [term]
  (comb-concat Succ term))

(defn Num
  "Defines Church numeral for `i`."
  [i]
  (cond
    (neg? i) nil
    (>= i 0) (loop [current 0
                    result Zero]
               (if (= current i)
                 result
                 (recur (inc current) (succ result))))))

(defn to-num
  "The given term is transformed to numeral or nil if it's not a numeral."
  [term]
  (let [red (weak-reduce (conj term 'f 'x) {:limit 1000})
        nof (count (filter #(= 'f %) (flatten red)))]
    (when (= red (weak-reduce (conj (Num nof) 'f 'x)))
      (list 'Num nof))))

(comment
  (Num 0)
  (Num 1)
  (to-num '[I])                                             ; => (Num 1)
  (Num 2)
  (Num 7)
  )

;; Definition of operators ----------------------------------------------------

;; Successor ------------------------------------------------------------------ 

;; see above
(comment
  (to-num (succ (Num 4)))                                   ; => (Num 5)
  )

;; Predecessor ----------------------------------------------------------------

(def Pred* '[S (K (S I (K (K I))))
             (S (S I (K (S (K (S (K (S (S I (K (K I))))) K))
                           (S (S (S I (K K)) (S I (K (K I))))
                              (S (K (S B)) (S I (K (K I))))))))
                (K (S (S I (K K)) (K (K I)))))])

;; How to come up with such a term? Simple answer: Stack Exchange!!
;; see https://math.stackexchange.com/questions/1794537/can-all-computable-numeric-functions-on-church-numerals-in-ski-combinator-calcul

(comment
  (time (weak-reduce (comb-concat Pred* (Num 0))))          ; => [K I] 2.7 secs
  (time (weak-reduce (comb-concat Pred* (Num 0)) {:trace true})) ; => [K I] 2.7 secs 17 steps
  (time (weak-reduce (comb-concat Pred* (Num 1))))          ; => [K I] 6.1 secs
  (time (weak-reduce (comb-concat Pred* (Num 1)) {:trace true})) ; => [K I] 6.1 secs 47 steps
  (time (weak-reduce (comb-concat Pred* (Num 2)) {:timeout 12000})) ; => [S B (K I)] 11.2 secs
  ;; extremely slow!!
  )

;; A little bit of thinking may help to understands, what's going on here!
;; Here is how to deduce a predecessor term:

;; Derivation of the predecessor combinator according to
;; Sören Stenlund: Combinators, λ-Terms and Proof Theory Chap 3.2

;; D a paring combinator that detects (Num 0)
(def D (abstract '[x y z] '[z (K y) x]))

(comment
  ; D x y (Num 0) = x
  (weak-reduce (comb-concat D '[x] '[y] (Num 0)))
  ; D x y (Num i) = y for i > 0
  (weak-reduce (comb-concat D '[x] '[y] (Num 5)))
  )

;; X_0 
(def X_0 (abstract '[x] (comb-concat D '[S B (x (K I))] '[x (K I)])))

;; the predecessor combinator
(def Pred' (abstract '[y] (comb-concat '[y] X_0 '[K (K I)] (Num 1))))

(comment
  (time (weak-reduce (comb-concat Pred' (Num 0))))          ; => [K I] 2.5 secs
  (time (weak-reduce (comb-concat Pred' (Num 0)) {:trace true})) ; => [K I] 2.5 secs 8 steps
  (time (weak-reduce (comb-concat Pred' (Num 1)) {:trace true})) ; => [K I] 210 secs 33 steps 
  (time (weak-reduce (comb-concat Pred' (Num 2))))          ; => [S B (K I)] 343 secs !!
  )

;; The result of Pred' is a term that is literally equal to (Num i)
;; but we can use the derivation in another way - by defining improper combinators:

(def-combinator '[D x y z] '[z (K y) x])
(def-combinator '[X0 x] '[D (S B (x (K I))) (x (K I))])
(def Pred (abstract '[y] ['y 'X0 '(K (K I)) '(S B (K I))]))
; or
(def-combinator '[P n] '[n X0 (K (K I)) (S B (K I))])

(defn pred
  "The predecessor of the given numeral."
  [term]
  (comb-concat '[P] term))

(defn pred'
  [term]
  (comb-concat Pred term))

(comment
  (weak-reduce '[P (K I)])                                  ; => [K I]
  (weak-reduce '[P (S B (K I))] {:trace true})              ; => [K I] 11 steps
  (weak-reduce '[P (S B (S B (K I)))] {:trace true})        ; => [S B (K I)] 17 steps

  (to-num (pred (Num 0)))
  (weak-reduce (conj (pred (Num 1)) 'f 'x))
  (weak-reduce (conj (pred (Num 1)) 'f 'x) {:trace true})
  (weak-reduce (conj (pred (Num 4)) 'f 'x) {:trace true})   ; =>  37 steps
  (weak-reduce (conj (pred (Num 5)) 'f 'x) {:limit 1000 :trace true}) ; => [f (f (f (f x)))] 45 steps
  (time (weak-reduce (conj (pred (Num 6)) 'f 'x) {:limit 1000 :trace true})) ; =>  0.2 secs 53 steps

  ;; Interesting:
  ;; In the variant Pred* or Pred' we get not many steps, but huge terms
  ;; In the variant with the improper combinators, we get a lot of steps
  ;; - first increasing size of terms and then a fast reduction in the size of the terms

  (to-num (pred (Num 1)))
  (to-num (pred (Num 2)))
  (to-num (pred (Num 3)))
  (to-num (pred (Num 4)))
  (time (to-num (pred (Num 4))))                            ; => (Num 3) 0.03 secs
  (time (to-num (pred (Num 5))))                            ; => (Num 4) 0.05 secs

  (to-num (pred' (Num 1)))
  (to-num (pred' (Num 2)))
  (to-num (pred' (Num 3)))
  (to-num (pred' (Num 4)))
  (time (to-num (pred' (Num 4))))                           ; => (Num 3) 0.3 secs
  (time (to-num (pred' (Num 5))))                           ; => (Num 4) 0.6 secs
  )

;; Addition -------------------------------------------------------------------

(def Add* '[S I (K (S B))])
(def Add '[B S (B B)])

(defn add
  "Adds two numerals."
  [term1 term2]
  (comb-concat Add term1 term2))

(comment
  ;; The following examples show that
  ;; Add* gives an expression that reduces to the literal form of the numeral
  ;; Add   is an expression does not reduce to the literal form of the numeral,
  ;; Add is much faster!
  (weak-reduce (comb-concat Add* (Num 0) (Num 2)))          ; => [S B (S B (K I))]
  (to-num (comb-concat Add* (Num 0) (Num 2)))               ; => (Num 2)
  ; but
  (weak-reduce (comb-concat Add (Num 0) (Num 2)))           ; => [S (B B (K I)) (S B (S B (K I)))]
  (to-num (comb-concat Add (Num 0) (Num 2)))                ; => (Num 2)

  (time (to-num (comb-concat Add* (Num 6) (Num 6))))        ; => 0.4 secs
  (time (to-num (comb-concat Add* (Num 7) (Num 6))))        ; => 0.8 secs

  (time (to-num (comb-concat Add (Num 6) (Num 6))))         ; => 0.2 secs
  (time (to-num (comb-concat Add (Num 7) (Num 6))))         ; => 0.4 secs
  (time (to-num (comb-concat Add (Num 7) (Num 7))))         ; => 0.7 secs
  )

;; Multiplication -------------------------------------------------------------

(def Mult* '[S (S (K S) (S (K (S I)) (S (K K) (S I (K (S B)))))) (K (K (K I)))])
(def Mult '[B])

(defn mult
  "Multiplies two numerals."
  [term1 term2]
  (comb-concat Mult term1 term2))

(comment
  ;; Observe how multiplication works
  (weak-reduce (comb-concat Mult* (Num 2) (Num 3) '[f] '[x]) {:trace true}) ; 71 steps
  (weak-reduce (comb-concat Mult (Num 2) (Num 3) '[f] '[x]) {:trace true}) ; 23 steps
  )

(comment
  ;; The following examples show that
  ;; Mult* gives an expression that reduces to the literal form of the numeral
  ;; Mult  is an expression does not reduce to the literal form of the numeral,
  ;; Mult is much faster!
  (weak-reduce (comb-concat Mult* '[S B (K I)] '[S B (K I)])) ; => [S B (K I)]
  (to-num (comb-concat Mult* '[S B (K I)] '[S B (K I)]))    ; => (Num 1)
  ; but
  (weak-reduce (comb-concat Mult '[S B (K I)] '[S B (K I)])) ; => [B (S B (K I)) (S B (K I))] 
  (to-num (comb-concat Mult '[S B (K I)] '[S B (K I)]))     ; => (Num 1)

  (weak-reduce (comb-concat Mult* '[K I] '[K I]))           ; => [K I]
  (to-num (comb-concat Mult* '[K I] '[K I]))                ; => (Num 0)
  ; but 
  (weak-reduce (comb-concat Mult '[K I] '[K I]))            ; => [ B (K I) (K I)]
  (to-num (comb-concat Mult '[K I] '[K I]))                 ; => (Num 0)

  (weak-reduce (comb-concat Mult* (Num 2) (Num 3)))         ; => [S B (S B (S B (S B (S B (S B (K I))))))]
  (time (to-num (comb-concat Mult* (Num 2) (Num 3))))       ; => (Num 6) 204 msecs - slow
  ; but
  (weak-reduce (comb-concat Mult (Num 2) (Num 3)))          ; => [B (S B (S B (K I))) (S B (S B (S B (K I))))]
  (time (to-num (comb-concat Mult (Num 2) (Num 3))))        ; => (Num 6) 17 msecs - fast
  )

(comment
  (to-num (mult (Num 0) (Num 2)))
  (to-num (mult (Num 2) (Num 2)))
  (to-num (mult (Num 2) (Num 3)))
  (to-num (mult (Num 2) (Num 6)))
  (time (to-num (mult (Num 2) (Num 7))))                    ; => 1.2 secs
  (time (to-num (mult (Num 2) (Num 8))))                    ; => 3.3 secs
  (time (to-num (mult (Num 2) (Num 9))))                    ; => 11.7 secs
  (time (to-num (mult (Num 2) (Num 10))))                   ; => 47.7 secs
  )
  