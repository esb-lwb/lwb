; lwb Logic WorkBench -- Propositional Logic SAT
; Examples: coloring of maps

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.coloring
  (:require [lwb.prop.cardinality :refer (oneof max-kof min-kof)])
  (:require [lwb.prop.sat :refer (sat true-only)]))

(defn color-atom
  "Makes a symbol from strings `country` and `color`."
  [country color]
  (symbol (str country "_" color)))

(defn unique-colors
  "Makes a sequence of clauses - expressing that each country has a
   unique color - from a collection of countries and colors."
  [countries colors]
  (mapcat oneof (partition (count colors)
      (for [country countries, color colors] (color-atom country color)))))

(defn adjacent
  "Makes a sequence of clauses for adjacent countries, i.e. that they
   have different colors."
  [country1 country2 colors]
  (let [symb1 (map #(color-atom country1 %) colors)
        symb2 (map #(color-atom country2 %) colors)]
     (map #(list 'or (list 'not %1) (list 'not %2)) symb1 symb2)))


; Example Australia

(def colors ["red" "green" "blue"])

; States and territories of Australia:
; 	wa = Western Australia,
;   nt = Northern Territory,
;   sa = South Australia,
;   q  = Queensland,
;   sw = New South Wales,
;   v  = Victoria

(def countries ["wa" "nt" "sa" "q" "sw" "v"])

(def neighbors [["wa" "nt"]
                ["wa" "sa"]
                ["nt" "sa"]
                ["nt" "q" ]
                ["q"  "sa"]
                ["q"  "sw"]
                ["sa" "sw"]
                ["sa" "v" ]
                ["sw" "v" ]])

(def coloring-australia
  (apply list 'and (concat (unique-colors countries colors) (mapcat #(adjacent (first %) (second %) colors) neighbors))))

(true-only (sat coloring-australia))
; => [nt_green q_blue sa_red sw_green v_blue wa_blue]