; lwb Logic WorkBench -- Präsentation
; Prädikatenlogik
; Copyright (c) 2017 - 2018 Burkhardt Renz, THM. All rights reserved.

(ns pres.pl
  (:require [lwb.pred :refer :all])
  (:require [lwb.pred.sat :refer [sat sat? valid?]])
  )


; Syntax ===========================

; Wenn man es genau nimmt, gibt es nicht _eine_ Sprache der Prädikatenlogik,
; sondern die Spraxche hängt von der Wahl der Prädikate und Funktionen ab.

; Man spricht von der Signatur und formuliert dann die Formeln in Bezug
; auf die Signatur.

; Beispiel einer Signatur

(def grp-sig
  {'unit [:func 0]
   'op   [:func 2]
   'inv  [:func 1]})

; Für welche algebraische Struktur eignet sich diese Signatur?

; Syntaktisch kommen für Formeln der Aussagenlogik hinzu:
; - Variablen und
; - Quantoren

; Beispiele für Formeln:

(def grp-ass '(forall [x y z] (= (op x (op y z)) (op (op x y) z))))
(wff? grp-ass grp-sig)

(def grp-unit '(forall [x] (= (op x unit) x)))
(wff? grp-unit grp-sig)

; aber
(wff? '(forall [x] (= (mult x unit x))) grp-sig)
; => false

(def grp-inv '(forall [x] (= (op x (inv x)) unit)))
(wff? grp-inv grp-sig)

(def grp-comm '(forall [x y] (= (op x y) (op y x))))
(wff? grp-comm grp-sig)

; Exkurs -----------------------------------

; Auch hier wird für die Implementierung clojure.spec eingesetzt
; dies hat nicht nur Vorteile:

; (wff? '(forall [x] (= (mult x unit x))) grp-sig :exception-if-not)
; Fehlermeldung versteht nur, wer spec gut kennt!

; Ende Exkurs --------------------------------


; Auswertung =================================

; Man muss nun ein Modell vorgeben, d.h.
; - ein Universum
; - Relationen für Prädikate und
; - Relationen for Funktionen

; Beispiel: Zyklische Gruppe der Ordnung 6

(def c6
  {:univ #{0 1 2 3 4 5}
   'op   [:func 2 #(mod (+ %1 %2) 6)]
   'inv  [:func 1 #(mod (- 6 %) 6)]
   'unit [:func 0 0]})

; Nun können wir Formeln auswerten

(eval-phi grp-ass c6)
; dauert etwas

(eval-phi grp-inv c6)
(eval-phi grp-unit c6)
(eval-phi grp-comm c6)

; okay: c6 ist eine Abelsche Gruppe

; Exkurs -----------------------------------

; Wie wird in der Prädikatenlogik ausgewertet?

; Da wir ein endliches Universum haben, können wir alle
; Quantoren auflösen:
; (forall ...) ergibt (and ...) wobei wir jedes Element des Universums in die Variablen einsetzen
; (exists ...) ergibt (or ...) wieder für alle möglichen Wahlen der Variablen

; So wird aus der Formel der Prädikatenlogik eine der Aussagenlogik und wir
; können den SAT-Solver einsetzen.

; Ende Exkurs --------------------------------

; Erfüllbarkeit =================================

; Das ist im Allgemeinen nicht lösbar
; aber wir beschränken auf endliche Universen und geben die Größe vor

; Beispiel:

; Gruppenaxiome
(def grp (list 'and grp-ass grp-unit grp-inv))

; Eine Gruppe der Ordnung 5 
(sat grp grp-sig 5)

; Wenn man genau hinschaut, sieht man, dass die Gruppe von :e1 erzeugt wird
; und zyklisch ist.

; Gibt es eine nicht-abelsche Gruppe der Ordnung 5?

(def grp-a (list 'and grp (list 'not grp-comm)))

(sat grp-a grp-sig 5)
; => nil

; und wie sieht es bei 6 Elementen aus?
(sat grp-a grp-sig 6)
; existiert!

; Exkurs -----------------------------------

; Zur Implementierung:

; Die Implementierung in lwb für die Erfüllbarkeit in der Prädikatenlogik
; verwendet einen Constraint-Solver, und zwar Kodkod (http://emina.github.io/kodkod/)
; (Derselbe Constraint-Solver, der auch in Alloy (http://alloy.mit.edu/alloy/) verwendet wird)

; Recht einfach:
; - transformiere die Formel in einen Kodkod-Ausdruck
; - lasse Kodkod ein Modell erzeugen
; - übersetze es zurück als Modell im Sinne von lwb

; Ende Exkurs --------------------------------

; Natürliches Schließen ======================

; pres.nd
