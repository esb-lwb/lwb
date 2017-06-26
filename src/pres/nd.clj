; lwb Logic WorkBench -- Präsentation
; Natürliches Schließen
; Copyright (c) 2017 Burkhardt Renz, THM. All rights reserved.

(ns pres.nd
  (:require [lwb.nd.repl :refer :all]))

; Aussagenlogik ===================================

(load-logic :prop)

; Die Regeln des natürlichen Schließens sind in einer Datei definiert,
; also nicht "fix reinprogrammiert"

; siehe resources/nd/rules-prop.edn
; Zu jedem Operator gibt es 
; - eine Regel ihn einzuführen (introduction)
; - eine Regel ihn zu eliminieren (elimination)

; lwb hat eine Maschine, in der man interaktiv die Regeln anwndet,
; dies geht vorwärts step-f
; und rückwärts step-b

; Beispiele

; Modus Tollens

(proof '[(impl P Q) (not Q)] '(not P))
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-f :not-e 2 4)

; Sätze kann man speichern
; dies ist für Modus Tollens schon geschehen,
; ich könnte es mir also auch einfach machen

(proof '[(impl P Q) (not Q)] '(not P))
(step-f :mt 1 2)

; Kontraposition

(proof '(impl P Q) '(impl (not Q) (not P)))
(step-b :impl-i 3)
(step-b :not-i 4)
(step-f :impl-e 1 3)
(step-b :not-e 6 2)

; Exkurs -----------------------------------
; Implementierung

; Clojure hat eine Bibliothek core.logic, die einen
; Auswerter für logische Formeln enthält. 
; Das Vorbild dieser Bibliothek ist minKanren, ein Logik-Werkzeug
; geschrieben in Scheme.

; lwb verwendet core.logic, um die Regeln auszuwerten:
; Beim Laden der Regeln wird pro Regel eine Funktion erzeugt,
; die von der logic engine von core.logic dann beim Anwenden
; der Regel ausgewertet wird. Solche Funktionen können logische Variablen enthalten,
; die dann substituiert werden. Substitutionen bei uns sind selbst wieder
; Teile der Formel, die man herlieten möchte.

; Ende Exkurs --------------------------------

; Prädikatenlogik =================================

(load-logic :pred)

; Auch hier sind die Regeln in einer Datei definiert
; Schwieriger wird die Sache, weil man dafür sorgen muss, dass
; überprüft wird, ob Substitutionen erlaubt sind.

; Beispiele

; De Morgan
; not-forall->exists-not

(proof '(not (forall [x] (P x))) '(exists [x] (not (P x))))
(step-b :raa 3)
(step-b :not-e 4 1)
(step-b :forall-i 4)
(swap '?1 :i)
(step-b :raa 5)
(step-b :not-e 6 2)
(step-b :exists-i 6 3)

; Ein bekanntes "Paradoxon"

; Für jede Gruppe von Professoren gilt:
; Es existiert einer, für den gilt, dass alle anderen betrunken sind, wenn er betrunken ist

; wir gehen davon aus, dass das Universum nicht leer ist

(proof '(exists [x] (impl (Drunk x) (forall [y] (Drunk y)))))
(step-f :tnd)
(swap '?1 '(forall [y] (Drunk y)))
(step-f :or-e 1 3)
(step-f :actual)
(swap '?2 :i)
(step-b :exists-i 5 3)
(step-b :impl-i 5)
(step-f :not-forall->exists-not 8)
(step-f :exists-e 9 11)
(swap '?3 :j)
(step-b :exists-i 13 10)
(step-b :impl-i 13)
(step-f :not-e 11 12)
(step-b :efq 15)

; Lineare temporale Logik ====================

; Interessanterweise war das natürliche Schließen in dieser Logik am
; schwierigsten zu programmieren. Grund: Wir müssen in die Anwendung der Regeln
; die Struktur der Modelle (eine lineare Kette) mit reinstecken!

(load-logic :ltl)

; Für die Formulierung brauchen wir eine Erweiterung:
; at gibt an, zu welchem Zeitpunkt eine Aussage gilt
; und wir haben Regeln, wie die Zustände aufeinander folgen dürfen
; siehe resources/rules-ltl.edn

; Ich habe alle Theoreme aus dem Buch von Kröger und Merz: Temporal Logic and State Systeme bewiesen
;
; not-atnext->atnext-not  Kröger/Merz T1

(proof '(at [i] (not (atnext A))) '(at [i] (atnext (not A))))
(step-f :succ)
(swap '?1 'i)
(swap '?2 'i')
(step-b :atnext-i 4 :? 2)
(step-b :not-i 4)
(swap '?3 'i')
(step-f :atnext-i 3 2)
(step-f :not-e 1 4)
(swap '?4 'i')

; Noch ein Beispiel:

; not-finally->always-not  Kröger/Merz T3

(proof '(at [i] (not (finally A))) '(at [i] (always (not A))))
(step-b :always-i 3)
(swap '?1 'j)   ; j ist ein beliebiger Zeitpunkt nach i
(step-b :not-i 4)
(step-f :finally-i 3 2)
(swap '?2 'i)
(step-b :not-e 6 1)

; Das war's
; Zeit für Fragen und Diskussion
