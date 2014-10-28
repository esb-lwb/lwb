; lwb Logic WorkBench -- Propositional Logic SAT
; Examples: sudoku

; Copyright (c) 2014 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.


(ns lwb.prop.examples.sudoku
  (:require [clojure.java.io :refer (reader)])
)

;; ## Format for sudoku in Clojure
;; We represent a puzzle and a solution as a 3x3 matrix
;; The value 0 stands for an unknown value in ther puzzle

;; ## Parser for files containing puzzles
;; The parsers looks for lines with 81 characters, the digits 1-9 and the character .    
;; Other lines in the file are ignored

(defn line2puzzle
  "Parses a line of 81 characters in ([1-9]|\\.) and returns a puzzle.    "
  [line]
  (into [] (->> line
                      (map #(if (= % \.) 0 (- (int %) 48)))
                      (partition 9)
                      (map #(into [] %)))))

; (line2puzzle "3...8.......7....51..............36...2..4....7...........6.13..452...........8..")
  
(defn parse
  "Parses file with filename and returns a list of puzzles."
  [filename]
  (with-open [rdr (reader filename)]
    (into '() (map line2puzzle (filter #(re-matches #"^([1-9]|\.){81}$" %) (line-seq rdr))))))

(parse "resources/sudoku/easy50.txt")
(parse "resources/sudoku/top95.txt")
