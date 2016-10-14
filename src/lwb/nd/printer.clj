; lwb Logic WorkBench -- Natural deduction

; Copyright (c) 2015 Tobias Völzel, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.printer
  (:require [lwb.nd.proof :refer [plid->plno]]
            [clojure.string :as str]
            [clojure.pprint :as pp]))

;; distance between left and right edge
(def distance 40)
;; length of the divider for subproofs
(def divider-length 25)

(defn pprint-line
  [proof depth item]
  (let [line (plid->plno proof (:plid item))
        body (str (if (= (:body item) :todo) "..." (:body item)))
        rule (condp = (:rule item)
               nil         ""
               :premise    "premise"
               :assumption "assumption"
               (let [name (subs (:rule item) 0 (.lastIndexOf (:rule item) "("))
                     ids  (subs (:rule item) (inc (.lastIndexOf (:rule item) "(")) (.lastIndexOf (:rule item) ")"))
                     ;; convert ids to line-numbers
                     lines   (str/replace ids #"\b[0-9]+\b" #(str (plid->plno proof (Integer. %))))
                     ;; [12 12] => [12]
                     lines-1 (str/replace lines #"\[([0-9]+)\s\1\]" #(str "[" (second %) "]"))
                     ;; sort the line-numbers asc
                     lines-2 (if (zero? (count lines-1))
                               ""
                               (str "(" (str/join " " (sort (str/split lines-1 #"\s+(?=[^\])}]*([\[({]|$))"))) ")"))
                                          user-inputs (subs (:rule item) (.lastIndexOf (:rule item) "["))
                                          user-inputs-1 (if (> (count user-inputs) 2)
                                                          (str " " user-inputs) nil)]
                                  (str name lines-2 user-inputs-1)))]
    (print (pp/cl-format nil "~3d: " line))
    (when (pos? depth)
      (print " ")
      (dotimes [_ depth] (print "| ")))
    (print body)
    (dotimes [_ (- distance (count body) (if (> depth 0) (inc (* 2 depth)) 0))] (print " "))
    (println rule)))

(defn pprint
  ([proof] (pprint proof proof 0))
  ([proof sub depth]
    (print "     ")
    (when (pos? depth)
      (print " ")
      (dotimes [_ (dec depth)] (print "| ")))
    (dotimes [_ (- divider-length depth)] (print "--"))
    (println)
    (doseq [item sub]
      (if (vector? item)
        (pprint proof item (inc depth))
        (pprint-line proof depth item)))
    (print "     ") 
    (when (pos? depth)
      (print " ")
      (dotimes [_ (dec depth)] (print "| ")))
    (dotimes [_ (- divider-length depth)] (print "--"))
    (println)))
      