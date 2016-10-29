; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.printer
  (:require [lwb.nd.proof :refer [plid plid->plno]]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]))

;; distance between left and right edge
(def distance 40)
;; length of the divider for subproofs
(def divider-length 25)

(defn- plid-plno-map
  [proof]
  (let [fp (flatten proof)]
    (into {} (map-indexed #(vector (:plid %2) (inc %1)) fp))))

(defn- pprint-line
  [proof depth pline plid-plno-map]
  (let [line (plid->plno proof (:plid pline))
        body (str (if (= (:body pline) :todo) "..." (:body pline)))
        refs (walk/postwalk-replace plid-plno-map (:refs pline))
        roth (str (:roth pline) " " refs)]
    (print (pp/cl-format nil "~3d: " line))
    (when (pos? depth)
      (print " ")
      (dotimes [_ depth] (print "| ")))
    (print body)
    (dotimes [_ (- distance (count body) (if (> depth 0) (inc (* 2 depth)) 0))] (print " "))
    (println roth)))

(defn pprint
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
