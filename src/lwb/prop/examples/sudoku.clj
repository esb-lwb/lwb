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

;; Configuration
(def n1
  "Order of the grid"
  3)
(def n2
  "Size of the units"
  (* n1 n1))
(def n4
  "Number of cells"
  (* n2 n2))

(def entries
  "Entries for cells"
  (map #(char (+ (int \0) %)) (range 1 (inc n2))))

;; A puzzle is represented as a vector of digits

;; ## Format for sudoku in Clojure
;; We represent a puzzle and a solution as a vector of n4 entries
;; or "." for an unknown value in the puzzle

;; ## Boolean encoding
;; We have n2 * n2 atoms, expressing whether a cell with coordinates 'row' and 'col'
;; has the value 'value'. The atoms are represented by symbols of the form 'cxyz',
;; with 'x' the row, 'y' the column and 'z' the value.

(defn- make-sym 
  "Makes a symbol from [row, col, value]."
  [[row col value]]
  (symbol (str "c" row col value)))

;; ## Rules of sudoku
;; The rules as a sequence of clauses are collected in 'rules-cl'

; Sequence of clauses expressing that each cell has exactly one value
(def cell-cl
  (let [cell-syms (partition n2 (for [r entries, c entries, v entries]
                 (make-sym [r c v])))]
    (mapcat oneof cell-syms)))
  
; Sequence of clauses expressing that each row has at most one of the values 1..n2
(def rows-cl
  (let [rows (partition n2 (for [r entries c entries] [r c]))
        syms (for [row rows v entries] (map #(make-sym (conj % v)) row ))]
    (mapcat #(max-kof 1 %) syms)))

; Sequence of clauses expressing that each col has at most one of the values 1..n2
(def cols-cl
  (let [cols (partition n2 (for [r entries c entries] [c r]))
        syms (for [col cols v entries] (map #(make-sym (conj % v)) col ))]
    (mapcat #(max-kof 1 %) syms)))

; Sequence of clauses expressing that each block has at most one of the values 1..n2
(def blk-cl
  (let [sects (let [s (partition n1 entries)] (for [s1 s, s2 s] (for [r s1 l s2] [r l])))
        syms (for [sect sects v entries] (map #(make-sym (conj % v)) sect))]
    (mapcat #(max-kof 1 %) syms)))

; Sequence of clauses expressing the rules of sudoku
(def rules-cl (concat cell-cl rows-cl cols-cl blk-cl))

;; ## Encoding a the givens as a sequence of clauses
(defn puzzle-cl
  "Clauses for the givens of the puzzle."
  [puzzle]
  (let [make-vec (fn [idx ch] (if (= ch \.) 
                                  nil 
                                  [(inc (quot idx n2)) (inc (rem idx n2)) (- (int ch) (int \0))]))
        make-cl  (fn [vec] (list 'or (make-sym vec))) ]
    (map make-cl (filter #(not (nil? %)) (map-indexed make-vec puzzle)))))

;; ## Combining a puzzle and the rules to a proposition
(defn sudoku-prop
  [puzzle]
  (apply list 'and (concat (puzzle-cl puzzle) rules-cl)))

;; ## Solving the puzzle

;; some helpers for transforming the result of sat
(defn true-only
  "Sequence of true atoms in an assignment vector"
  [assign-vec]
  (loop [vec assign-vec, result []]
    (if (empty? vec)
      result
      (let [atom (first vec) value (second vec)]
        (recur (subvec vec 2) (if value (conj result atom) result))))))

(defn true-vec2solution
  "Solution from vector of true atoms"
  [true-vec]
  (let [vec (sort true-vec)]
    (mapv #(nth (name %) 3) vec)))

(defn solve
  "Solve Sudoku puzzle"
  [puzzle]
  (->> puzzle
       (sudoku-prop)
       (sat)
       (true-only)
       (true-vec2solution)))

;; ## Pretty-printing puzzles and solutions

(defn pretty-print
  "Pretty-printing Sudoku of order 3."
  [puzzle]
  (let [rule "+-------+-------+-------+\n"]
    (doseq [[row col ch] (map-indexed #(vector (inc (quot %1 n2)) (inc (rem %1 n2)) %2) puzzle)]
      (if (and (= 1 col) (= 1 (mod row n1))) (print rule))
      (if (= 1 (mod col n1)) (print (str "| " ch " ")) (print (str ch " ")))
      (if (= 9 col) (print "|\n"))
      )
    (print rule)))

stop -- the following is the interactive part

;; Sequence of clauses for the rules of Sudoku

rules-cl

;; ## Example of a puzzle
(def puzzle (vec ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83."))

puzzle

(pretty-print puzzle)

(pretty-print (solve puzzle))

;; ## Parser for files containing puzzles
;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .    
;; Other lines in the file are ignored

(defn parse
  "Parses file with filename and returns a list of puzzles."
  [filename]
  (with-open [rdr (reader filename)]
    (into () (map vec (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr))))))

;; ## Benchmarks
(defn bench
 [puzzles]
 (time
  (do
    (dorun (map solve puzzles))
    :done)))

;; easy50.txt
(def easy50 (parse "resources/sudoku/easy50.txt"))

easy50

(dotimes [_ 10]
  (bench easy50))
;=> 6.5 secs

;; top95.txt
(def top95 (parse "resources/sudoku/top95.txt"))

top95

(dotimes [_ 10]
  (bench top95))
;=> 12 secs

;; hardest.txt
(def hardest (parse "resources/sudoku/hardest.txt"))

(dotimes [_ 10]
  (bench hardest))
;=> 1.4 secs

