; lwb Logic WorkBench -- Natural deduction
; Storage for rules and theorems

; Copyright (c) 2016 Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.storage)

(def roths 
  "Global storage for rules and theorems"
  (atom{}))

(defn reset-roths
  "Resets the internal storage for rules"
  [] (reset! roths {}))

(def theorems
  (atom {}))
