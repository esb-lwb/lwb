; lwb Logic WorkBench -- Präsentation
; Kombinatorische Logik
; Copyright (c) 2021 Burkhardt Renz, THM. All rights reserved.

(ns pres.cl
  (:require [lwb.cl :refer :all]
            [lwb.cl.repl :refer :all]))



; Syntax ===========================

; 1. Kombinatoren werden repräsentiert als Symbole beginnend
;    mit einem Großbuchstaben, z.B. S, K, I.
; 2. Variablen werden repräsentiert durch Symbole beginnend
;    mit einem Kleinbuchstaben, z.B. x, y.
; 3. Funktionsapplikation wird repräsentiert durch eine Liste
;    mit zwei Elementen, z.B. ((K x) y).
; 4. Ein Term wird durch einen Vektor dargestellt, in einem Term
;    kann man die Klammern weglassen entsprechend der Konvention
;    dass Funktionsapplikation linksassoziativ ist, z.B.
;    [K x y], [S x y z], [(((S x) y) z)].
; 5. Ein S-Term ist ein Symbol oder eine geschachtelte Liste von
;    Funktionsapplikationen, z.B. (((S x) y) z)

(wff? '[S])
; => true

(wff? '[S x y z])
; => true

(wff? '[(S)])
; => false

(wff? '[(S K)])
; => true

(wff? '[S K I])
; => true


; Kombinatoren =======================

; Kombinatoren sind nicht eingebaut, sondern werden definiert:


(def-combinator '[I x] '[x] "Identitätsfunktion")
(def-combinator '[K x y] '[x] "Konstanzfunktion")
(def-combinator '[S x y z] '[x z (y z)] "Verschmelzungsfunktion")
(show-combinators)

; Church-Rosser

;; Figure on cover

(session '[S (S x y z)(S x y z) z])
(red :S 2)
(red :S 1)
(red :S 1)
; --- Current session -----------------------------
; 1: [S (S x y z) (S x y z) z]    [:given]
; 2: [S (x z (y z)) (S x y z) z]  [:red :S 2]
; 3: [x z (y z) z (S x y z z)]    [:red :S 1]
; 4: [x z (y z) z (x z (y z) z)]  [:red :S 1]
; -------------------------------------------------

(session '[S (S x y z)(S x y z) z])
(red :S 2)
(red :S 2)
(red :S 1)
; --- Current session -------------------------------
; 1: [S (S x y z) (S x y z) z]      [:given]
; 2: [S (x z (y z)) (S x y z) z]    [:red :S 2]
; 3: [S (x z (y z)) (x z (y z)) z]  [:red :S 2]
; 4: [x z (y z) z (x z (y z) z)]    [:red :S 1]
; ---------------------------------------------------

(session '[S (S x y z)(S x y z) z])
(red :S 1)
(red :S 1)
(red :S 1)
; --- Current session -----------------------------
; 1: [S (S x y z) (S x y z) z]    [:given]
; 2: [S x y z z (S x y z z)]      [:red :S 1]
; 3: [x z (y z) z (S x y z z)]    [:red :S 1]
; 4: [x z (y z) z (x z (y z) z)]  [:red :S 1]
; -------------------------------------------------

(session '[S (S x y z)(S x y z) z])
(red :S 1)
(red :S 2)
(red :S 1)
; --- Current session -----------------------------
; 1: [S (S x y z) (S x y z) z]    [:given]
; 2: [S x y z z (S x y z z)]      [:red :S 1]
; 3: [S x y z z (x z (y z) z)]    [:red :S 2]
; 4: [x z (y z) z (x z (y z) z)]  [:red :S 1]
; -------------------------------------------------

(session '[S (S x y z)(S x y z) z])
(red :S 3)
(red :S 2)
(red :S 1)
; --- Current session -------------------------------
; 1: [S (S x y z) (S x y z) z]      [:given]
; 2: [S (S x y z) (x z (y z)) z]    [:red :S 3]
; 3: [S (x z (y z)) (x z (y z)) z]  [:red :S 2]
; 4: [x z (y z) z (x z (y z) z)]    [:red :S 1]
; ---------------------------------------------------

(session '[S (S x y z)(S x y z) z])
(red :S 3)
(red :S 1)
(red :S 1)
; --- Current session -----------------------------
; 1: [S (S x y z) (S x y z) z]    [:given]
; 2: [S (S x y z) (x z (y z)) z]  [:red :S 3]
; 3: [S x y z z (x z (y z) z)]    [:red :S 1]
; 4: [x z (y z) z (x z (y z) z)]  [:red :S 1]
; -------------------------------------------------

; Fixpunkt-Satz

;; Curry's fixed point combinator
(session '[S (B W B) (B W B) x])
(red :S)
(red :B)
(red :B)
(red :W)
(red :B)
(exp :B 3)
(exp :B 3)
(exp :S)