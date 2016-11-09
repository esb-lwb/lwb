; lwb Logic WorkBench -- Natural deduction 

; Copyright (c) 2015 - 2016 Tobias Völzel, Burkhardt Renz, THM. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php).
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.

(ns lwb.nd.prereqs
  (:require [lwb.pred.substitution :refer [substitution]]))

;; # Functions used in prerequisites of rules

(defn substitution?
  "Is `phi'` the substitution of `var` in `phi`?    
   Throws exception if that's not the case."
  [phi' phi var term]
  (if (= phi' (substitution phi var term))
    true
    (throw (Exception. (format "%s should be the substitution of %s in %s by %s" phi' var phi term)))))

