; lwb Logic WorkBench -- Propositional Logic bdd

; Copyright (c) 2016 Mathias Gutenbrunner, Jens Lehnhäuser and Burkhardt Renz, THM.
; All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.bdd
  (:require [lwb.prop :refer :all])
  (:import  (net.sf.javabdd JFactory BDD)))

; Example
(def phi1 '(and (or a b) (not (or c d))))

; function from prop
(atoms-of-phi phi1)

(defn atoms-as-bdds
  "the JavaBDD Factory `bddf` returns a map of BDD Objects for each atom of formula `phi`."
  [bddf phi]
  (let [atoms (atoms-of-phi phi), c (count atoms)]
    (.setVarNum bddf c)
    (zipmap atoms (map #(.ithVar bddf %) (range c)))))

; TODO: extend such that we have all operators and even trivial cases like true, false
(def functions
  {'or #(.or %1 %2)
   'and #(.and %1 %2)
   'xor #(.xor %1 %2)
   'not #(.not %1)
   'ite #(.ite %1 %2 %3)
   'impl #(.imp %1 %2)})

(defn build-bdd
  [atom-map phi]
  (if (symbol? phi)
    (get atom-map phi)
    (let [op (first phi)]
      (case (arity op)
        1 ((functions op) (build-bdd atom-map (second phi)))
        2 ((functions op) (build-bdd atom-map (second phi))
            (build-bdd atom-map (nth phi 2)))
        3 ((functions op) (build-bdd atom-map (second phi))
            (build-bdd atom-map (nth phi 2))
            (build-bdd atom-map (nth phi 3)))
        -1 (reduce (functions op) (map #(build-bdd atom-map %1) (rest phi)))
        ))))

; TODO: represent the table as a clojure object
(defn bdd-table
  [phi nodenum]
  (let [bddf (JFactory/init nodenum 10000)
        atom-map (atoms-as-bdds bddf phi)
        bdd  (build-bdd atom-map phi)]
    (.printTable bddf bdd)
    (.done bddf)))

(bdd-table phi1 100000)

; TODO: use the clojure representation of a bdd to generate dot code
(defn bdd-dot
  [phi nodenum]
  (let [bddf (JFactory/init nodenum 10000)
        atom-map (atoms-as-bdds bddf phi)
        bdd  (build-bdd atom-map phi)]
    (.printDot bdd)
    (.done bddf)))

(bdd-dot phi1 100000)

; TODO: sat with bdd
