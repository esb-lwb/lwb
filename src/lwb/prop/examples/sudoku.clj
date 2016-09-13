; lwb Logic WorkBench -- Propositional Logic SAT
; Examples: sudoku

; Copyright (c) 2014 - 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.sudoku
  (:require [clojure.java.io :refer (reader)])
  (:require [lwb.prop.cardinality :refer (oneof max-kof min-kof)])
  (:require [lwb.prop.sat :refer (sat true-only)])
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

(def digits
  "Entries for cells"
  (map #(char (+ (int \0) %)) (range 1 (inc n2))))

;; A puzzle is represented as a vector of digits

;; ## Format for sudoku in Clojure
;; We represent a puzzle and a solution as a vector of n4 entries
;; or "." for an unknown value in the puzzle

;; ## Boolean encoding
;; We have n2 * n2 atoms, expressing whether a cell with coordinates 'row' and 'col'
;; has the value 'value'. The atoms are represented by symbols of the form 'cxyd',
;; with 'x' the row, 'y' the column and 'd' the value.

(defn- make-sym 
  "Makes a symbol from [row, col, value]."
  [[row col value]]
  (symbol (str "c" row col value)))

;; ## Rules of sudoku
;; The rules as a sequence of clauses are collected in 'rules-cl'

; Sequence of clauses expressing that each cell has exactly one value
(def cell-cl
  (let [cell-syms (partition n2 (for [x digits, y digits, d digits] (make-sym [x y d])))]
    (mapcat oneof cell-syms)))

; Sequence of clauses expressing that each row has at most one of the values 1..n2
(def rows-cl
  (let [rows (partition n2 (for [r digits c digits] [r c]))
        syms (for [row rows d digits] (map #(make-sym (conj % d)) row ))]
    (mapcat #(max-kof 1 %) syms)))

; Sequence of clauses expressing that each col has at most one of the values 1..n2
(def cols-cl
  (let [cols (partition n2 (for [r digits c digits] [c r]))
        syms (for [col cols d digits] (map #(make-sym (conj % d)) col ))]
    (mapcat #(max-kof 1 %) syms)))

; Sequence of clauses expressing that each block has at most one of the values 1..n2
(def blk-cl
  (let [blks (let [s (partition n1 digits)] (for [s1 s, s2 s] (for [r s1 l s2] [r l])))
        syms (for [blk blks d digits] (map #(make-sym (conj % d)) blk))]
    (mapcat #(max-kof 1 %) syms)))

; Sequence of clauses expressing the rules of sudoku
(def rules-cl (concat cell-cl rows-cl cols-cl blk-cl))

;; ## Encoding a the givens as a sequence of clauses
(defn puzzle-cl
  "Clauses for the givens of the puzzle."
  [puzzle]
  (let [make-vec (fn [idx ch] (when-not (= ch \.) 
                                  [(inc (quot idx n2)) (inc (rem idx n2)) (- (int ch) (int \0))]))
        make-cl  (fn [vec] (list 'or (make-sym vec))) ]
    (map make-cl (remove nil? (map-indexed make-vec puzzle)))))

;; ## Combining a puzzle and the rules to a proposition
(defn sudoku-prop
  [puzzle]
  (apply list 'and (concat (puzzle-cl puzzle) rules-cl)))

;; ## Solving the puzzle

;; some helpers for transforming the result of sat
(defn solution
  "Solution from model"
  [model]
  (let [vec (sort (true-only model))]
    (mapv #(nth (name %) 3) vec)))

(defn solve
  "Solve Sudoku puzzle"
  [puzzle]
  (-> puzzle
       (sudoku-prop)
       (sat)
       (solution)))

;; ## Pretty-printing puzzles and solutions

(defn pretty-print
  "Pretty-printing Sudoku of order 3."
  [puzzle]
  (let [ruler "+-------+-------+-------+\n"]
    (doseq [[row col ch] (map-indexed #(vector (inc (quot %1 n2)) (inc (rem %1 n2)) %2) puzzle)]
      (if (and (= 1 col) (= 1 (mod row n1))) (print ruler))
      (if (= 1 (mod col n1)) (print (str "| " ch " ")) (print (str ch " ")))
      (if (= 9 col) (print "|\n"))
      )
    (print ruler)))

;stop -- the following is the interactive part

(comment
  ;; Sequence of clauses for the rules of Sudoku

  rules-cl
  (count rules-cl)

  ;; ## Example of a puzzle
  (def puzzle (vec ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83."))

  puzzle

  (lwb.prop/cnf? (sudoku-prop puzzle))

  (pretty-print puzzle)

  (pretty-print (solve puzzle))

  ;; ## Parser for files containing puzzles
  ;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .    
  ;; Other lines in the file are ignored

  (defn parse
    "Parses file with filename and returns a list of puzzles."
    [filename]
    (with-open [rdr (reader filename)]
      (vec (map vec (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr))))))

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
  ;=> 137 msecs per puzzle

  ;; top95.txt
  (def top95 (parse "resources/sudoku/top95.txt"))

  top95

  (dotimes [_ 10]
    (bench top95))
  ;=> 141 msecs per puzzle

  ;; hardest.txt
  (def hardest (parse "resources/sudoku/hardest.txt"))

  (dotimes [_ 10]
    (bench hardest))
  ;=> 137 msecs per puzzle

  ; average 139 msecs per puzzle

  (def p88 (nth top95 88))
  (def p92 (nth top95 92))

  (- 81 (count (filter zero? p88)))
  ; => 17
  (time (solve p88))
  ; 445 msecs

  (pretty-print p88)
  (pretty-print (solve p88))

  (- 81 (count (filter zero? p92)))
  ; => 17
  (time (solve p92))
  ; 186 msecs
  ) ; end comment
