; lwb Logic WorkBench -- Präsentation
; Kombinatorische Logik
; Copyright (c) 2021 Burkhardt Renz, THM. All rights reserved.

(ns pres.cl
  (:require [lwb.cl :refer :all]
            [lwb.cl.repl :refer :all]))



; I.1 Syntax 

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

(wff? '[S K K])
; => true

(wff? '[S I I (S I I)])
; => true

(wff? '[S (S x y z) (S x y z) z])
; => true

(wff? '[(S) K])
; => false

; --------------------------------------------------------------

; I.2 Definition der Kombinatoren 

; Kombinatoren sind nicht eingebaut, sondern werden definiert:


(def-combinator '[I x] '[x])
(def-combinator '[K x y] '[x])
(def-combinator '[S x y z] '[x z (y z)])
(show-combinators)

; --- Defined combinators --------
; I  := [I x] -> [x]             
; K  := [K x y] -> [x]           
; S  := [S x y z] -> [x z (y z)] 
; --------------------------------

; --------------------------------------------------------------

; I.3 Interaktive Session

(session '[S (K (S I)) K a b])
(red :S)
(red :K)
(red :S)
(red :I)
(red :K)

; Wir haben a und b vertauscht

; --------------------------------------------------------------

; I.4 Reduktion

(one-step-red '[S (S x y z) b c] :S)
; => [S x y z c (b c)]

(one-step-red '[S (S x y z) b c] :S 2)
; => [S (x z (y z)) b c]

; --------------------------------------------------------------

; I.5 Expansion 

(one-step-exp '[x z (y z) c (b c)] :S)
; => [S (x z (y z)) b c]

(one-step-exp '[x z (y z) c (b c)] :S 2)
; => [S x y z c (b c)]

(one-step-exp '[x] :K)
; => [K x _0] 

; --------------------------------------------------------------

; II.1 Interessante Beispiele

(weak-reduce '[S (S x y z) (S x y z) z])
; => [x z (y z) z (x z (y z) z)]

(weak-reduce '[S (S x y z) (S x y z) z] {:trace true})
; Immer die erste Möglichkeit der Reduktion

(weak-reduce '[S I I (S I I)])
; gibt einen sehr langen Ausdruck, warum?

(meta (weak-reduce '[S I I (S I I)]))
;{:reduced ((I (I (I (I ((S I) I))))) (I (I (I (I (I (I (I (I (I (I (I (I (I ((S I) I))))))))))))))),
; :no-steps 100,
; :cycle :unknown,
; :overrun true}

; Es gibt aber auch eine ganz andere Möglichkeit

(session '[S I I (S I I)])
(red :S)
(red :I)
(red :I)
; => [S I I (S I I)] ein Zyklus

(weak-reduce '[S I I (S I I)] {:limit 3 :trace true})
; In Zeile 2 wird mit :S reduziert, nicht mit :I wie oben

; Zwei Kombinatoren, gleicher Effekt

(weak-reduce '[S K K x])

(weak-reduce '[I x])

; I = S K K im Sinne der extensionalen Gleichheit

; Zwei Kombinatoren in Normalform

(weak-reduce '[S K K])

(weak-reduce '[S K S])

; aber
(weak-reduce '[S K K x])
; => [x]

(weak-reduce '[S K S x])
; => [x]

; S K K = S K S im Sinne der extensionalen Gleichheit 


; --------------------------------------------------------------

; II.2 Normalform

(weak-reduce '[S (S x y z) (S x y z) z])
; => [x z (y z) z (x z (y z) z)]

; --------------------------------------------------------------

; II.3 Church-Rosser 

(session '[S (S x y z) (S x y z) z])
(red* :S [1 1 1])

(session '[S (S x y z) (S x y z) z])
(red* :S [3 2 1])

; und alle weiteren 4 Möglichkeiten führen zum
; gleichen Term

; --------------------------------------------------------------

; II.4 Currys Fixpunkt-Kombinator

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
; => [x (S (B W B) (B W B) x)]

; --------------------------------------------------------------

; II.5 Bracket Abstraction

; Gesucht ein Kombinator, der zwei Argumente vertauscht

(abstract '[x y] '[y x])
; => [S (K (S I)) K]

; Probe mit a und b

(weak-reduce (conj (abstract '[x y] '[y x]) 'a 'b))
; => [b a]


; --------------------------------------------------------------

; III Symullyans Rätsel interaktiv lösen

(def-combinatory-birds)
(show-combinators)

; Aufgabe 1 aus Kapitel 11 von Smullyans Buch

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

; Alle Rätsel, die man interaktiv lösen kann
; auf github: src/lwb/cl/examples/smullyan.clj

; --------------------------------------------------------------

; IV.1 Boolesche Werte und Operatoren der Aussagenlogik

; Definition Wahrheitswerte

(def True '[K])
(def False '[K I])

(defn to-boolean
  [term]
  (let [red (weak-reduce (conj term 't 'f))]
    (cond
      (= red '[t]) 'True
      (= red '[f]) 'False
      :else nil))) 

; Definition von not

(def Not '[S (S I (K (K I))) (K K)])

(defn Not'
  [term]
  (to-boolean (weak-reduce (comb-concat Not term))))

; Wahrheitstafel von not

(Not' True)
(Not' False)

; Definition von and

(def And '[S S K])

(defn And'
  [term1 term2]
    (to-boolean (weak-reduce (comb-concat And term1 term2))))

  ; Wahrheitstafel von and
  
(And' True True)
(And' True False)
(And' False True)
(And' False False)

; --------------------------------------------------------------

; IV.2 Natürliche Zahlen: Church-Numerale

; Null

(def Zero '[K I])

; Nachfolger-Kombinator

(def Succ '[S B]) 

; Nachfolger-Funktion

(defn succ
  "The successor of the given numeral.
   Pre: `term` is a numeral."
  [term]
  (comb-concat Succ term))

; Numeral

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

(Num 0)
(Num 1)
(Num 2)

(defn to-num
  "The given term is transformed to numeral or nil if it's not a numeral."
  [term]
  (let [red (weak-reduce (conj term 'f 'x) {:limit 1000})
        nof (count (filter #(= 'f %) (flatten red)))]
    (when (= red (weak-reduce (conj (Num nof) 'f 'x)))
      (list 'Num nof))))

; Addition

(def Add '[S I (K (S B))])

(defn add
  "Adds two numerals."
  [term1 term2]
  (comb-concat Add term1 term2))

(add (Num 1) (Num 2))

(to-num (add (Num 1) (Num 2)))
; => (Num 3)

; --------------------------------------------------------------

; V. Das Titelbild von Bimbós Monographie


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
(red* :S [2 2 1])

(session '[S (S x y z)(S x y z) z])
(red* :S [1 1 1])

(session '[S (S x y z)(S x y z) z])
(red* :S [1 2 1])

(session '[S (S x y z)(S x y z) z])
(red* :S [3 2 1])

(session '[S (S x y z)(S x y z) z])
(red* :S [3 1 1])

