; lwb Logic WorkBench -- Präsentation
; Aussagenlogik
; Copyright (c) 2017 Burkhardt Renz, THM. All rights reserved.

(ns pres.al
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.sat :refer [sat sat? valid?]]
            [clojure.spec.alpha :as s])
  )

; Syntax ===========================

; In Lehrbüchern wird üblicherweise Infix-Notation verwendet,
; also z.B. ¬P ∨ Q ∧ R um Aussagen zu formulieren
;
; Damit man Klammern sparen kann, gibt es Regeln für die Präzedenz
; von Operatoren, etwa ¬ bindet am stärksten, ∧ bindet stärker als ∨
;
; Die schlägt sich im Syntaxbaum nieder, der dadurch eindeutig wird
; und in unserem Beispiel so aussehen würde
;
;                   ∨
;                  /   ∖
;                ¬       ∧
;                ⎮     /   ∖
;                P   Q       R

; Die Syntax in lwb ist lispy, d.h. wir schreiben einfach den
; abstrakten Syntaxbaum selbst hin:

; In unserem Beispiel:
;
; (or (not P) (and Q R))

(def phi '(or (not P) (and Q R)))

; Warum '??

(wff? phi)

; aber

(wff? '(or not P (and Q R)))

; Operators in der Aussagenlogik

; `not`:   Negation - unär
; `and`:   Konjunktion  -- n-är
; `or`:    Disjunktion  -- n-är
; `impl`:  Implikation  -- binär
; `equiv`: Äquivalenz  -- binär
; `xor`:   Exklusives or -- binär
; `ite`:   If-then-else  -- ternär

; Syntaxbaum kann man auch hübsch zeichnen

(texify phi "beispiel")

; Exkurs -----------------------------------
; Wie ist wff? implementiert? 

; wff? verwendet clojure.spec

; Ein einfacher Ausdruck ist true/false oder atomare Aussage
(s/def ::simple-expr (s/or :bool boolean? :atom atom?))

; Ein zusammengesetzter Ausdruck ist eine Liste mit einem Operator und
; Formeln als Argumente (deren Zahl zur Arität des Operators passt)

(defn- arity-ok? [{:keys [op params]}]
  (let [arity (arity op)]
    (if (= arity -1) true
                     (= arity (count params)))))

(s/def ::compl-expr (s/and list? (s/& (s/cat :op op? :params (s/* ::fml)) arity-ok?)))

; Eine ordentliche Formel der Aussagenlogik:
(s/def ::fml (s/or :simple-expr ::simple-expr
                   :compl-expr ::compl-expr))

; Ende Exkurs --------------------------------

; Auswertung =================================

; Man gibt einfach Wahrheitswerte vor


(def m1 {'P true 'Q false 'R true})

(eval-phi phi m1)
; => false

(def m2 {'P false 'Q false 'R true})

(eval-phi phi m2)
; => true

; Exkurs -----------------------------------
; Implementierung

; verwendet die Funktion eval von Clojure
; Die Formel ist ein Ausdruck, der mit dem gegebenen Modell
; ausgewertet wird 

; Data is Code, Code is Data = Homoikonizität

; Ende Exkurs --------------------------------

; Erfüllbarkeit =================================

; Wahrheitstafel (brute force)

(ptt phi)

; viel besser: SAT-Solver

(sat phi)

(sat phi :all)

; Exkurs -----------------------------------
; Implementierung

; Tseitin-Transformation in eine Formel in CNF, die
; erfüllbarkeits-äquvalent ist
; Umbauen ins dimac-Format
; Aufruf von SAT4J (www.sat4j.org)
; Ergebnis in unsere Sprache zrurcü übersetzen

; Ende Exkurs --------------------------------

; Beispiel sudoku 

; lwb.prop.examples.sudoku

; Natürliches Schließen ======================

; pres.nd
