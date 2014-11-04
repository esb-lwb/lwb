; lwb Logic WorkBench -- Propositional Logic SAT
; Examples: sudoku

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.examples.sudoku
  (:require [clojure.java.io :refer (reader)])
  (:require [lwb.prop.cardinality :refer (oneof max-kof min-kof)])
  (:require [lwb.prop.sat :refer (sat)])
)

(def n1 2)
(def n2 4)
(def n4 16)
(def nums (range 1 (inc n2)))

;; ## Format for sudoku in Clojure
;; We represent a puzzle and a solution as a string of 81 digits
;; or "." for an unknown value in the puzzle

;; ## Boolean encoding
;; We have 9 * 9 atoms, expressing whether a cell with coordinates 'row' and 'col'
;; has the value 'value'. The atoms are represented by symbols of the form 'cxyz',
;; with 'x' the row, 'y' the column and 'z' the value.

(defn- make-sym 
  "Makes a symbol from [row, col, value]."
  [[row col value]]
  (symbol (str "c" row col value)))

#_(make-sym [1 1 9])

;; ## Rules of sudoku
;; The rules as a sequence of clauses are collectoed in the Var 'rules'

; Sequence of clauses expressing that each cell has exactly one value
(def cell-cl
  (let [cell-syms (partition n2 (for [r nums c nums v nums]
                 (make-sym [r c v])))]
    (mapcat oneof cell-syms)))
  
cell-cl

; Sequence of clauses expressing that each row has at most one of the values 1..n2
(def rows-cl
  (let [rows (partition n2 (for [r nums c nums] [r c]))
        syms (for [row rows v nums] (map #(make-sym (conj % v)) row ))]
    (mapcat #(max-kof 1 %) syms)))

rows-cl

; Sequence of clauses expressing that each col has at most one of the values 1..n2
(def cols-cl
  (let [cols (partition n2 (for [r nums c nums] [c r]))
        syms (for [col cols v nums] (map #(make-sym (conj % v)) col ))]
    (mapcat #(max-kof 1 %) syms)))

cols-cl

           
; Sequence of clauses expressing that each sector has at most one of the values 1..9
(def sects-cl 
  (let [sects (let [s (partition n1 nums)] (for [s1 s, s2 s] (for [r s1 l s2] [r l])))
        syms (for [sect sects v nums] (map #(make-sym (conj % v)) sect))]
    (mapcat #(max-kof 1 %) syms)))

sects-cl

; Sequence of clauses expressing the rules of sudoku
(def rules-cl (concat cell-cl rows-cl cols-cl sects-cl))

;; ## Encoding a puzzle as a sequence of clauses

(defn puzzle-cl
  "Clauses for the puzzle."
  [puzzle]
  (let [make-vec (fn [idx ch] (if (= ch \.) 
                                  nil 
                                  [(inc (quot idx n2)) (inc (rem idx n2)) (- (int ch) (int \0))]))
        make-cl  (fn [vec] (list 'or (make-sym vec))) ]
    (map make-cl (filter #(not (nil? %)) (map-indexed make-vec puzzle)))))

(puzzle-cl "1...2...3...4...")
;(puzzle-cl "3...8.......7....51..............36...2..4....7...........6.13..452...........8..")


;; ## Combining a puzzle and the rules to a proposition
(defn sudoku-prop
  [puzzle]
  (apply list 'and (puzzle-cl  puzzle) rules-cl))

(sudoku-prop "1...2...3...4...")
(class (sudoku-prop "1...2...3...4..."))
;(sudoku-prop "3...8.......7....51..............36...2..4....7...........6.13..452...........8..")

;; ## Solving the puzzle
(defn solve 
  [puzzle]
  (sat (sudoku-prop puzzle)))

(solve "1...2...3...4...")
;(solve "3...8.......7....51..............36...2..4....7...........6.13..452...........8..")


(class (cons 'and(max-kof 1 '[p q r]) 'and))
(sat (oneof '[p q r]))
(sat (apply list 'and (min-kof 1 '[p q r])))

;; ## Parser for files containing puzzles
;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .    
;; Other lines in the file are ignored


  
(defn parse
  "Parses file with filename and returns a list of puzzles."
  [filename]
  (with-open [rdr (reader filename)]
    (into () (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr)))))

#_(parse "resources/sudoku/easy50.txt")
#_(parse "resources/sudoku/top95.txt")

(defn puzzle-cl
  "Clauses for the puzzle."
  [puzzle]
  (let [make-vec (fn [idx ch] (if (= ch \.) 
                                  nil 
                                  [(inc (quot idx 9)) (inc (rem idx 9)) (- (int ch) (int \0))]))
        make-cl  (fn [vec] (list 'or (make-sym vec))) ]
    (map make-cl (filter #(not (nil? %)) (map-indexed make-vec puzzle)))))

#_(puzzle-cl "3...8.......7....51..............36...2..4....7...........6.13..452...........8..")
