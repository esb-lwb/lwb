; lwb Logic WorkBench -- Predicate logic

; Copyright (c) 2017 - 2018 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.pred.examples.sat
  (:require [lwb.pred.sat :refer :all]))

;; Propositional formulae
(def prop-sig {'P [:pred 0] 'Q [:pred 0]})

(sat 'P prop-sig 3)
(sat '(and P Q) prop-sig 2)
(sat '(and P (not P)) prop-sig 2)

;; First definition of a group
;; Signature of a group in the spirit of the universal algebra
(def grp-sig
  {'unit [:func 0]
   'op   [:func 2]
   'inv  [:func 1]})

;; Group axioms
;; Associativity of group operation
(def grp-ass '(forall [x y z] (= (op x (op y z)) (op (op x y) z))))

;; Unit
(def grp-unit '(forall [x] (= (op x unit) x)))

;; Inverses
(def grp-inv '(forall [x] (= (op x (inv x)) unit)))

;; Abelian Group?
(def grp-comm '(forall [x y] (= (op x y) (op y x))))

(def grp (list 'and grp-ass grp-unit grp-inv))

(sat grp grp-sig 1)
(sat grp grp-sig 2)
(sat grp grp-sig 2 :all)
(sat grp grp-sig 3 :all)
(sat grp grp-sig 4 :all)
(sat grp grp-sig 5 :all)

; Non-abelian groups
(def grp-na (list 'and grp (list 'not grp-comm)))

(count (sat grp-na grp-sig 2 :all))
(count (sat grp-na grp-sig 3 :all))
(count (sat grp-na grp-sig 4 :all))
(count (sat grp-na grp-sig 5 :all))
; => 0
(comment
  (count (sat grp-na grp-sig 6 :all))
  ; => 20 after some secs
  )

(sat grp-na grp-sig 6 :one)

;; Second, classic definition of a group
;; Signature of a group according to the classical definition
(def grp-sig-classic
  {'op [:func 2]})

(def grp-axioms-classic
  '(and
     (forall [x y z] (= (op x (op y z)) (op (op x y) z)))
     (exists [unit] (and
                      (forall [x] (= (op x unit) x))
                      (forall [x] (exists [inv] (= (op x  inv) unit)))))))

(sat grp-axioms-classic grp-sig-classic 1 :all)
(sat grp-axioms-classic grp-sig-classic 2 :all)
(sat grp-axioms-classic grp-sig-classic 3 :all)
(sat grp-axioms-classic grp-sig-classic 4 :all)
(sat grp-axioms-classic grp-sig-classic 5 :all)
(comment
  (count (sat grp-axioms-classic grp-sig-classic 6 :all))
  )

; Non-abelian groups
(def grp-na-classic (list 'and grp-axioms-classic (list 'not grp-comm)))

(count (sat grp-na-classic grp-sig-classic 2 :all))
(count (sat grp-na-classic grp-sig-classic 3 :all))
(count (sat grp-na-classic grp-sig-classic 4 :all))
(count (sat grp-na-classic grp-sig-classic 5 :all))
; => 0
(comment
  (count (sat grp-na-classic grp-sig-classic 6 :all))
  ; => 120 after some minutes
  )

(sat grp-na-classic grp-sig-classic 6 :one)
; fast result


(comment
  ;; Another signature for the definition of a group
  ;; see: Kenneth Kunen: "Single Axioms for Groups", Technical Report University of Wisconsin, February 1992
  (def grp-sig-2
    {'op  [:func 2]
     'inv [:func 1]})

  (def grp-s1 '(forall [x y w z] (= x (op w (inv (op (op (inv (op (inv y) (op (inv w) x))) z) (inv (op y z))))))))

  (sat grp-s1 grp-sig-2 2 :all)
  (sat grp-s1 grp-sig-2 3 :all)

  (def grp-axioms-2
    '(and
       (forall [x y z] (= (op x (op y z)) (op (op x y) z)))
       (exists [unit] (and
                        (forall [x] (= (op x unit) x))
                        (forall [x] (= (op x (inv x)) unit))))))

  (sat grp-axioms-2 grp-sig-2 2 :all)

  (def kunen
    (list 'equiv grp-s1 grp-axioms-2))

  (sat? kunen grp-sig-2 2)
  (valid? kunen grp-sig-2 2)

  (valid? kunen grp-sig-2 5)
  ; => true (after 1 minute or so)

  ;(time (valid? kunen grp-sig-2 6))
  ; "Elapsed time: 805705.043977 msecs"
  ; => true

  ;; but
  (def jian
    (list 'equiv grp-s1 grp))

  (sat? jian grp-sig 2)
  (valid? jian grp-sig 2)

  ; Counter example
  (sat (list 'not jian) grp-sig 2 :all)
  ; => #{{:unit #{[:unit]},
  ;       :op #{[:e1 :unit :unit] [:e1 :e1 :e1] [:unit :e1 :unit] [:unit :unit :e1]},
  ;       :inv #{[:unit :unit] [:e1 :e1]}}}
  (sat? grp-s1 grp-sig 2)
  ; => true  A group with 2 elements fulfills grp-s1
  (valid? (list 'impl grp-s1 grp) grp-sig 2)
  ; => false grp-s1 does not imply the group axioms in the signature with 3 fns
  (valid? (list 'impl grp-s1 grp-axioms-2) grp-sig-2 2)
  ; => true  grp-s1 implies the group axioms in the signature with 2 fns

  ;; This exmaples falsifies the claim of Zhang Jian in the paper 
  ;; "Finite Model Searching: An Introduction one Some Personal Notes" 
  ;; that the equation grp_s1 does not axiomatize groups.
  ;; This was shown by B.H. Neumann "Yet Another Single Law for Groups" 
  ;; Illinois Journal of Mathematics Vol . 30, No. 2, Summer 1986

  ;; Jian thinks that a model finder constructs a counter example even for
  ;; the smallest example of a group with 2 elements
  ;; Jian is wrong: He uses the signature with :op, :inv and :unit to construct
  ;; the examples, but Neuman uses the signature that does just have :op and :inv,
  ;; which are the only symbols in grp-s1.

  ;; Jian's confusion is disentangled by Kenneth Kunen in his paper
  ;; Kenneth Kunen: "Single Axioms for Groups", Technical Report University of Wisconsin, February 1992
  ;; (Kunen does not mention Jian explicitly)
  )
