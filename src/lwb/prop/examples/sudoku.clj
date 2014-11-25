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

; n1 can be 2 or 3
; the restriction is caused by our choice of a digit string as the
; representation of a puzzle
(def n1 3)
(def n2 (* n1 n1))
(def n4 (* n2 n2))
(def nums (range 1 (inc n2)))

;; ## Format for sudoku in Clojure
;; We represent a puzzle and a solution as a string of n4 digits
;; or "." for an unknown value in the puzzle

;; ## Boolean encoding
;; We have n2 * n2 atoms, expressing whether a cell with coordinates 'row' and 'col'
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

           
; Sequence of clauses expressing that each sector has at most one of the values 1..n2
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


(def demopuzzle ".24...38.6.72.91.481.7.3.96.48...97...........69...51.75.9.8.414.16.57.9.96...83.")


;; ## Combining a puzzle and the rules to a proposition
(defn sudoku-prop
  [puzzle]
  (apply list 'and (concat (puzzle-cl  puzzle) rules-cl)))

(sudoku-prop demopuzzle)

;; ## Solving the puzzle

;; some helper for transforming the result of sat
(defn true-only
  "Sequence of true atoms in an assignment vector"
  [assign-vec]
  (loop [vec assign-vec, result []]
    (if (empty? vec)
      result
      (let [atom (first vec) value (second vec)]
        (recur (subvec vec 2) (if value (conj result atom) result))))))

(defn true-vec2solution
  "Solution string from vector of true atoms"
  [true-vec]
  (let [vec (sort true-vec)]
    (apply str (map #(nth (name %) 3) vec))))


(defn solve
  "Solve Sudoku puzzle"
  [puzzle]
  #_(sat (sudoku-prop puzzle))
  (->> puzzle
       (sudoku-prop)
       (sat)
       (true-only)
       (true-vec2solution)))

#_(solve "1...2...3...4...")

(solve demopuzzle)

;; ## Pretty-printing puzzles and solutions

(defn pretty-print
  [puzzle]
  (let [rule "+-----+-----+-----+\n"]
    (doseq [[row col ch] (map-indexed #(vector (inc (quot %1 n2)) (inc (rem %1 n2)) %2) puzzle)]
      (if (and (= 1 col) (= 1 (mod row n1))) (print rule))
      (cond (= 1 (mod col n1)) (print (str "|" ch))
            (= 2 (mod col n1)) (print (str " " ch " "))
            (= 0 (mod col n1)) (print ch)
            )
      (if (= 9 col) (print "|\n"))
      )
    (print rule)))




(pretty-print demopuzzle)

(pretty-print (solve demopuzzle))

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