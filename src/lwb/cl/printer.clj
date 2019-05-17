; lwb Logic WorkBench -- Combinatory logic - Printing

; Copyright (c) 2019  Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.cl.printer
  (:require [clojure.string :as str]))

; -----------------------------------------------------------------------------
;; # Printing the defined combinators

(defn print-combs-item
  [item length-id]
  (format (str "%-" length-id "s := %s -> %s") (name (key item)) (:redex (val item)) (:effect (val item))))

(defn print-comb-nickname
  [item]
  (let [n (:nickname (val item))]
    (if n (str "  '" n "'") "")))

(defn length-item
  [combs-value]
  (let [r-length
        (->> combs-value vals (map #(count (str (:redex %)))) (apply max))
        e-length
        (->> combs-value vals (map #(count (str (:effect %)))) (apply max))]
    (+ r-length e-length)))

(defn print-comb
  [key combs-value]
  (let [item (find combs-value key)]
    (if item
      (println (str (print-combs-item item (count (name key))) (print-comb-nickname item)))
      (println (str "No combinator with key '" key "' defined.")))))


(defn print-combs
  [combs-value]
  (if (empty? combs-value)
    (println "There are no combinators defined yet!")
    (let [length-id (apply max (map #(count (str %)) (keys combs-value)))
          length-item (length-item combs-value)
          length (- (+ 2 length-id length-item) (count "Defined combinators"))]
      (println (str "--- Defined combinators ---" (str/join (repeat length "-"))))
      (doseq [item combs-value]
        (println (format (str "%-" (+ 8 length-id length-item) "s %s") (print-combs-item item length-id) (print-comb-nickname item))))
        
      (println (str "---------------------------" (str/join (repeat length "-")))))))

; -----------------------------------------------------------------------------
;; Printing the current session 

(defn print-session-line
  [line length]
  (format (str "%5d: %-" length "s %s") (:lno line) (:term line) (:rule line)))

(defn print-session
  [session-store-val]
  (if (empty? session-store-val)
    (println "There is no current session!\nUse (session term) to start one.")
    (let [length (inc (apply max (map #(count (str (:term %))) session-store-val)))]
      (println (str "--- Current session -" (str/join (repeat length "-"))))
      (doseq [line session-store-val]
        (println (print-session-line line length)))
      (println (str "---------------------" (str/join (repeat length "-")))))))

    

