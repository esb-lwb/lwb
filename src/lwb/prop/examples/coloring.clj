; lwb Logic WorkBench -- Propositional Logic SAT
; Examples: coloring of maps

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.coloring
  (:require [lwb.prop.cardinality :refer (oneof max-kof min-kof)])
  (:require [lwb.prop.sat :refer (sat)]))

(defn color-atom
  "Makes a symbol from strings `country` and `color`."
  [country color]
  (symbol (str country "_" color)))

(color-atom "nt" "green")

(defn unique-colors
  "Makes a sequence of clauses - expressiong that each country has a
   unique color - from a collection of countries and colors."
  [countries colors]
  (map #(oneof %) (map #(color-atom countries %) colors)))

; States an terrirories of Australia:
; 	wa = Western Australia,
;   nt = Northern Territory,
;   sa = South Australia,
;   q  = Queensland,
;   sw = New South Wales,
;   v  = Victoria

(unique-colors ["wa" "nt"] ["red" "green" "blue"])

(def colors ["red" "green" "blue"])
(def countries ["wa" "nt"])

(map oneof (map #(color-atom "wa" %) colors))