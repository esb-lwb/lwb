; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.printer
  (:require [lwb.nd.proof :refer [plid->plno]]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]))

;; # Printing the proof

;; ## Printing to the REPL's stdout

(def distance
  "Distance between left and right edge."
  40)

(def divider-length
  "Length of the divider for subproofs."
  25)

(defn- plid-plno-map
  "Mapping of ids of pline to line numbers."
  [proof]
  (let [fp (flatten proof)]
    (into {} (map-indexed #(vector (:plid %2) (inc %1)) fp))))

(defn- pprint-line
  "Prints a proof line."
  [proof depth pline plid-plno-map]
  (let [line (plid->plno proof (:plid pline))
        body (str (if (= (:body pline) :todo) "..." (:body pline)))
        refs (walk/postwalk-replace plid-plno-map (:refs pline))
        roth (str " "(:roth pline) " " refs)]
    (print (pp/cl-format nil "~3d: " line))
    (when (pos? depth)
      (print " ")
      (dotimes [_ depth] (print "| ")))
    (print body)
    (dotimes [_ (- distance (count body) (if (pos? depth) (inc (* 2 depth)) 0))] (print " "))
    (println roth)))

(defn pprint
  "Prints the proof."
  ([proof] (pprint proof proof 0))
  ([proof sub depth]
   (let [smap (plid-plno-map proof)]
     (print "     ")
     (when (pos? depth)
       (print " ")
       (dotimes [_ (dec depth)] (print "| ")))
     (dotimes [_ (- divider-length depth)] (print "--"))
     (println)
     (doseq [pline sub]
       (if (vector? pline)
         (pprint proof pline (inc depth))
         (pprint-line proof depth pline smap)))
     (print "     ")
     (when (pos? depth)
       (print " ")
       (dotimes [_ (dec depth)] (print "| ")))
     (dotimes [_ (- divider-length depth)] (print "--"))
     (println))))

;; ## Generating TeX-Code
