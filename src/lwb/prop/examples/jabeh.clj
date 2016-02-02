; lwb Logic WorkBench -- Propositional Logic SAT
; Examples: jabeh

; Copyright (c) 2016 Burkhardt Renz, Marco Stephan, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.prop.examples.jabeh
  (:require [lwb.prop :refer :all])
  (:require [lwb.prop.cardinality :refer (min-kof kof)])
  (:require [lwb.prop.sat :refer (sat)])
  (:require [clojure.core.matrix :refer (emap)])
  (:require [clojure.string :as str]))

;; The puzzle is given by a map with the keys :col-holes,
;; :row-holes and :field
;; e.g.
(comment
  (def p
    {:col-holes [2 2 1 2 1 1 1 2]
     :row-holes [1 1 2 1 2 2 2 1]
     :field     [[u u u 2 u u u u]
                 [u u u u 6 u u u]
                 [u u u u u 5 u 7]
                 [u u 4 u 7 u u u]
                 [1 u u 4 6 8 u u]
                 [u u u u u u u u]
                 [u u u 1 u u 7 u]
                 [u u u u u u u u]]})
  )

;; :col-holes is a vector of the number of holes in the columns
;; :row-holes is a vector of the number of holes in te rows
;; :field is the field with
;; u = an unknown state of the cell
;; a number = an arrow, decoding the direction at cell x as follows
;;             7 8 1
;;             6 x 2
;;             5 4 3 

;; The encoding of the puzzle in the propositional logic:
;; We represent each cell as an atomic proposition  build from the
;; index of the cell by make-sym.
;; If the atom is true, there is a hole, false otherwise

;; The constraints are
;; - the number of true atoms in a row is given by the corresponding number in :row-holes,
;; - the number of true atoms in a columns is given by the corresponding number in :col-holes,
;; - the atom for a cell with an arrow is false, and
;; - in the direction of the arrow one of the corresponding atoms has to be true

(def u
  "A symbol for an unknown state of a cell in the puzzle"
  \.)

;; Symbols representing the proposition that a cell
;; is a "hole" or not.
;; The symbols are good for puzzle with up to 100 rows
;; and 100 columns

(defn- make-sym
  "Makes a symbol from an index [row, col]."
  [[row col]]
  (symbol (format "c%02d%02d" row col)))

(defn- size
  "Size [row-count col-count] of a (well-formed) puzzle."
  [puzzle]
  [(count (:row-holes puzzle)) (count (:col-holes puzzle))])

;; Generating a seq of clauses for the constraints on the rows

(defn rows-cl
  "Seq of clauses for the rows: each row has exactly the number of
   holes given by the corresponding number in :row-holes."
  [puzzle]
  (let [[row-count col-count] (size puzzle)
        syms (map make-sym (for [r (range row-count) c (range col-count)] [r c]))
        sym-rows (partition col-count syms)]
    (mapcat kof (:row-holes puzzle) sym-rows)))

;; Generating a seq of clauses for the constraints on the columns

(defn cols-cl
  "Seq of clauses for the columns: each column has exactly the number of
   holes given by the corresponding number in :col-holes."
  [puzzle]
  (let [[row-count col-count] (size puzzle)
        syms (map make-sym (for [c (range col-count) r (range row-count)] [r c]))
        sym-rows (partition row-count syms)]
    (mapcat kof (:col-holes puzzle) sym-rows)))


;; Generating a seq of clauses for the constraints given by arrows

(defn- next-cell
  "Next cell at [r c] in the given direction."
  [puzzle direction [r c]]
  (let [[row-count col-count] (size puzzle)
        [r2 c2] (case direction
                  1 [(dec r) (inc c)]
                  2 [r (inc c)]
                  3 [(inc r) (inc c)]
                  4 [(inc r) c]
                  5 [(inc r) (dec c)]
                  6 [r (dec c)]
                  7 [(dec r) (dec c)]
                  8 [(dec r) c])]
    (cond
      (neg? r2) nil
      (neg? c2) nil
      (>= r2 row-count) nil
      (>= c2 col-count) nil
      :else [r2 c2])))

(defn- next-cells
  "All cells at [r c] in the given direction."
  [puzzle direction [r c]]
  (into () (drop 1 (take-while some?
                               (iterate (partial next-cell puzzle direction) [r c])))))

(defn- arrow-cl
  "Seq of clause for the fact that there is at least one hole
   in the cells in the direction of the arrow at [r c]."
  [puzzle direction [r c]]
  (min-kof 1 (map make-sym (next-cells puzzle direction [r c]))))

(defn- arrow-cell?
  "Checks whether [[row col] value] is a cell with a value that's an arrow."
  [[[row col] value]]
  (not= value u))

(defn- neg-sym [sym]
  "Clause that negates sym."
  (list (list 'or (list 'not sym))))

(defn- arrow-cl-complete
  "Seq of clauses for the arrow-cell itself and for the cells in its direction."
  [puzzle [[row col] value]]
  (let [sym (make-sym [row col])
        neg (neg-sym sym)]
    (concat neg (arrow-cl puzzle value [row col]))))

(defn arrows-cl
  "Seq of clauses for all arrows, i.e. a clause of a single literal,
  expressing that the cell with the arrow itself is not a hole 
  together with the clause that there is a hole in the direction
  of the arrow."
  [puzzle]
  (let [[row-count col-count] (size puzzle)
        field (:field puzzle)
        cells (for [r (range row-count) c (range col-count)] [[r c] (get-in field [r c])])
        arrow-cells (filter arrow-cell? cells)]
    (mapcat (partial arrow-cl-complete puzzle) arrow-cells)))

;; Combining a puzzle and the rules to a proposition

(defn jabeh-prop
  "Generates the proposition for the given puzzle."
  [puzzle]
  (apply list 'and
         (concat (rows-cl puzzle) (cols-cl puzzle) (arrows-cl puzzle))))

(defn solve
  "Solve Jabeh puzzle."
  [puzzle]
  (-> puzzle
      (jabeh-prop)
      (sat)))

;; Constructing the solution  with + for holes and - else

(defn- prepare-solution
  "Prepares the solution from the sat solver to
  a matrix with + (a hole) and - (not a hole)."
  [puzzle solution]
  (let [[_ col-count] (size puzzle)]
    (->> solution
       (partition 2)
       (map vec)
       (sort)
       (map #(if (= (second %) true) \+ \-))
       (partition col-count)
       (map vec))))

(defn- combine
  "Combines the given field with the prepared solution."
  [field prepared-sol]
  (let [map-fn (fn [a b] (if (= a \.) b a))]
    (emap map-fn field prepared-sol)))

(defn solve-and-assoc
  "Solve and bind in field of the puzzle"
  [puzzle]
  (let [psol (prepare-solution puzzle (solve puzzle))
        fsol (combine (:field puzzle) psol)]
    (assoc puzzle :field fsol)))

;; Pretty Printer, good for puzzle with up to 10 rows and columns

(defn- print-cell
  [cell]
  (case cell
    1 \u2197
    2 \u2192
    3 \u2198
    4 \u2193
    5 \u2199
    6 \u2190
    7 \u2196
    8 \u2191
    \- \u25cb
    \+ \u25cf
    \.
    ))

(defn pretty-print
  "Pretty-printing jabeh."
  [puzzle]
  (let [[row-count col-count] (size puzzle)]
    (do
      (print "Puzzle\n")
      (print (str "  | " (str/join " " (:col-holes puzzle)) "\n"))
      (print (str "--+" (apply str (repeat col-count "--")) "\n"))
      (doseq [row (range row-count) col (range col-count)]
        (if (= col 0) (print (str (nth (:row-holes puzzle) row) " | ")))
        (print (str (print-cell (get-in (:field puzzle) [row col])) " "))
        (if (= (dec col-count) col) (print "\n"))))))


(defn print-puzzle-and-solution
  [puzzle]
  (pretty-print puzzle)
  (pretty-print (solve-and-assoc puzzle)))

;; Examples

(comment
  (defn test-puzzle [file]
    (print-puzzle-and-solution (load-file (str "resources/jabeh/" file ".edn"))))

  (test-puzzle "jabeh01")
  ved
  (test-puzzle "jabeh02")
  (test-puzzle "jabeh03")
  (test-puzzle "jabeh04")
  (test-puzzle "jabeh05")
  (test-puzzle "jabeh06")
  (test-puzzle "jabeh07")


  ;; Benchmarks
  ;; load, run, print
  (defn bench
    [puzzles]
    (time
      (do
        (dorun (map test-puzzle puzzles))
        :done)))

  (dotimes [_ 10]
    (bench ["jabeh01" "jabeh02" "jabeh03" "jabeh04" "jabeh05" "jabeh06"]))
  ; => 180 msec / 6 = 30 msec per puzzle

  )
