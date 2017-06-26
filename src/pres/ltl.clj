; lwb Logic WorkBench -- Präsentation
; Lineare temporale Logik
; Copyright (c) 2017 Burkhardt Renz, THM. All rights reserved.

(ns pres.ltl
  (:require [lwb.ltl :refer :all])
  (:require [lwb.ltl.eval :refer :all])
  (:require [lwb.ltl.kripke :as kripke])
  (:require [lwb.ltl.sat :refer [sat sat? valid?]])
  )

; Syntax ===========================

; In der linearen temporalen Logik gehen wir daon aus, dass wir mehrere Zustände haben,
; die aufeinander folgen und in jedem Zustand gelten Aussagen.

; Zusätzliche Operatoren über die Aussagenlogik hinaus sind:
;
 
; always | gilt in jedem Zustand | unär
; atnext | gilt im nächsten Zustand | unär
; finally | gilt irgendwann mal | unär
; until | phi gilt bis psi wahr wird | binär
; release | psi bleibt wahr bis (einschließlich) first phi wahr wird | binär

; Beispiele

(def phi1 '(finally wantY))
(wff? phi1)

(def phi2 '(until zero crit))
(wff? phi2)

(def phi3 '(finally (always wantY)))
(wff? phi3)

(def phi4 '(impl (atnext (not wantY)) (atnext (atnext wantY))))
(wff? phi4)

(texify phi4 "beispiel")

; Auswertung =================================

; Man muss ein Modell vorgeben.
; Für die LTL ist dies eine Kripke-Struktur

; Beispiel (aus den Übungen von Bodo Igler)
;

(def bodos {:atoms   '#{zero crit wantY}
            :nodes   {:q_0 '#{zero}
                      :q_1 '#{crit wantY}
                      :q_2 '#{wantY}
                      :q_3 '#{crit}
                      :q_4 '#{zero crit wantY}} 
            :initial :q_0
            :edges   #{[:q_0 :q_1]
                       [:q_0 :q_3]
                       [:q_1 :q_3]
                       [:q_2 :q_3]
                       [:q_3 :q_1]
                       [:q_3 :q_2]
                       [:q_3 :q_4]
                       [:q_4 :q_3]}})

(kripke/texify bodos "beispiel")

(eval-phi phi1 bodos)
; => true

(eval-phi phi2 bodos)
; => true

(eval-phi phi3 bodos)
; => false

(eval-phi phi4 bodos)
; => true

; Exkurs -----------------------------------
; Hinweis zur Implementierung

; 1. Man konstruiert einen Büchi-Automaten zur gegebenen Kripke-Struktur
; 2. Man negiert die Formel phi und übersetzt sie in einen Büchi-Automaten, der sie erkennt
; 3. Nun bildet man das synchronisierte Produkt der beiden Automaten. 
;    Ein erfolgreicher Lauf  zu einem Endzustand entspricht einem Lauf in der
;    Kripke-Struktur, der (not phi) erfüllt.
;    Also: Ist das Produkt leer, ist phi wahr.

; lwb benutzt zur Transformation einer Formel in einen Büchi-Automaten
; die Bibliothek ltl2buchi (siehe https://ti.arc.nasa.gov/profile/dimitra/)

; Ende Exkurs --------------------------------

; Erfüllbarkeit =================================

; Wir müssen zu einer gegebenen Formel eine Kripke-Struktur erzeugen
; Eigentlich haben das im Abschnitt zur Auswertung mit Hilfe von ltl2buchi schon getan.

phi1 

(sat phi1)

(kripke/texify (sat phi1) "beispiel")

phi4

(sat phi4)

(kripke/texify (sat phi4) "beispiel")

; Natürliches Schließen ======================

; pres.nd
